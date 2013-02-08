#===============================================================================
#                                                                              #
# Author    :  Angus Johnson                                                   #
# Version   :  5.1.0                                                           #
# Date      :  1 February 2013                                                 #
# Website   :  http://www.angusj.com                                           #
# Copyright :  Angus Johnson 2010-2013                                         #
#                                                                              #
# License:                                                                     #
# Use, modification & distribution is subject to Boost Software License Ver 1. #
# http://www.boost.org/LICENSE_1_0.txt                                         #
#                                                                              #
# Attributions:                                                                #
# The code in this library is an extension of Bala Vatti's clipping algorithm: #
# "A generic solution to polygon clipping"                                     #
# Communications of the ACM, Vol 35, Issue 7 (July 1992) PP 56-63.             #
# http://portal.acm.org/citation.cfm?id=129906                                 #
#                                                                              #
# Computer graphics and geometric modeling: implementation and algorithms      #
# By Max K. Agoston                                                            #
# Springer; 1 edition (January 4, 2005)                                        #
# http://books.google.com/books?q=vatti+clipping+agoston                       #
#                                                                              #
# See also:                                                                    #
# "Polygon Offsetting by Computing Winding Numbers"                            #
# Paper no. DETC2005-85513 PP. 565-575                                         #
# ASME 2005 International Design Engineering Technical Conferences             #
# and Computers and Information in Engineering Conference (IDETC/CIE2005)      #
# September 24-28, 2005 , Long Beach, California, USA                          #
# http://www.me.berkeley.edu/~mcmains/pubs/DAC05OffsetPolygon.pdf              #
#                                                                              #
#===============================================================================

import math
from collections import namedtuple
from decimal import Decimal

# decimal.getcontext().prec = 14
horizontal = Decimal('-Infinity')

class ClipType: (Intersection, Union, Difference, Xor) = range(4)
class PolyType:    (Subject, Clip) = range(2)
class PolyFillType: (EvenOdd, NonZero, Positive, Negative) = range(4)
class JoinType: (Square, Round, Miter) = range(3)
class EdgeSide: (Left, Right) = range(2)
class Protects: (Neither, Left, Right, Both) = range(4)
class Direction: (RightToLeft, LeftToRight) = range(2)

Point = namedtuple('Point', 'x y')

class LocalMinima(object):
    y = 0
    leftBound = rightBound = nextLm = None
    def __init__(self, y, leftBound, rightBound):
        self.y = y
        self.leftBound = leftBound
        self.rightBound = rightBound

class Scanbeam(object):
    __slots__ = ('y','nextSb')
    def __init__(self, y, nextSb = None):
        self.y = y
        self.nextSb = nextSb
    def __repr__(self):
        s = 'None'
        if self.nextSb is not None: s = '<obj>'
        return "(y:%i, nextSb:%s)" % (self.y, s)

class IntersectNode(object):
    __slots__ = ('e1','e2','pt','nextIn')
    def __init__(self, e1, e2, pt):
        self.e1 = e1
        self.e2 = e2
        self.pt = pt
        self.nextIn = None

class OutPt(object):
    __slots__ = ('idx','pt','prevOp','nextOp')
    def __init__(self, idx, pt):
        self.idx = idx
        self.pt = pt
        self.prevOp = None
        self.nextOp = None

class OutRec(object):
    __slots__ = ('idx','bottomPt','isHole','FirstLeft', 'pts','PolyNode')
    def __init__(self, idx):
        self.idx = idx
        self.bottomPt = None
        self.isHole = False
        self.FirstLeft = None
        self.pts = None
        self.PolyNode = None

#===============================================================================
# Unit global functions ...
#===============================================================================
def IntsToPoints(ints):
    result = []
    for i in range(0, len(ints), 2):
        result.append(Point(ints[i], ints[i+1]))
    return result

def Area(polygon):
    # see http://www.mathopenref.com/coordpolygonarea2.html
    highI = len(polygon) - 1
    A = (polygon[highI].x + polygon[0].x) * (polygon[0].y - polygon[highI].y)
    for i in range(highI):
        A += (polygon[i].x + polygon[i+1].x) * (polygon[i+1].y - polygon[i].y)
    return float(A) / 2

#===============================================================================
# PolyNode & PolyTree classes (+ ancilliary functions)
#===============================================================================
class PolyNode(object):
    """Node of PolyTree"""
    Contour = []
    Parent = None
    Index = 0
    ChildCount = 0
    Childs = []
    def IsHole(self):
        result = True
        while (self.Parent is not None):
            result = not result
            self.Parent = self.Parent.Parent
        return result
    def GetNext(self):
        if (self.ChildCount > 0):
            return self.Childs[0]
        else:
            return self._GetNextSiblingUp()
    def _AddChild(self, node):
        self.Childs.append(node)
        node.Index = self.ChildCount
        node.Parent = self
        self.ChildCount += 1
    def _GetNextSiblingUp(self):
        if (self.Parent is None):
            return None
        elif (self.Index == self.Parent.ChildCount - 1):
            return self.Parent._GetNextSiblingUp()
        else:
            return self.Parent.Childs[self.Index +1]

class PolyTree(PolyNode):
    """Container for PolyNodes"""
    _AllNodes = []
    def Clear(self):
        self._AllNodes = []
        self.Childs = []
        self.ChildCount = 0
    def GetFirst(self):
        if (self.ChildCount > 0):
            return self.Childs[0]
        else:
            return None
    def Total(self):
        return len(self._AllNodes)

def _AddPolyNodeToPolygons(polynode, polygons):
    """Internal function for PolyTreeToPolygons()"""
    if (len(polynode.Contour) > 0):
        polygons.append(polynode.Contour)
    for i in range(polynode.ChildCount):
        _AddPolyNodeToPolygons(polynode.Childs[i], polygons)

def PolyTreeToPolygons(polytree):
    result = []
    _AddPolyNodeToPolygons(polytree, result)
    return result

#===============================================================================
# ClipperBase class (+ data structs & ancilliary functions)
#===============================================================================

def _PointsEqual(pt1, pt2):
    return (pt1.x == pt2.x) and (pt1.y == pt2.y)

def _SlopesEqual(pt1, pt2, pt3, pt4 = None):
    if pt4 is None:
        return (pt1.y-pt2.y)*(pt2.x-pt3.x) == (pt1.x-pt2.x)*(pt2.y-pt3.y)
    else:
        return (pt1.y-pt2.y)*(pt3.x-pt4.x) == (pt1.x-pt2.x)*(pt3.y-pt4.y)

def _SlopesEqual2(e1, e2):
    return e1.deltaY * e2.deltaX == e1.deltaX * e2.deltaY

class Edge(object):
    xBot = yBot = xCurr = yCurr = xTop = yTop = tmpX = 0
    dx = Decimal(0); deltaX = deltaY = 0
    polyType = PolyType.Subject; side = EdgeSide.Left
    windDelta = windCnt = windCnt2 = 0; outIdx = -1
    nextE = prevE = nextInLML = prevInAEL = nextInAEL = prevInSEL= nextInSEL = None
    def __repr__(self):
        return "(%i,%i -> %i,%i {dx:%0.2f} %i {%x})" % \
            (self.xBot, self.yBot, self.xTop, self.yTop, self.dx, self.outIdx, id(self))

def _SetDx(e):
    e.deltaX = e.xTop - e.xBot
    e.deltaY = e.yTop - e.yBot
    if e.deltaY == 0: e.dx = horizontal
    else: e.dx = Decimal(e.deltaX)/Decimal(e.deltaY)

def _SwapSides(e1, e2):
    side =    e1.side
    e1.side = e2.side
    e2.side = side

def _SwapPolyIndexes(e1, e2):
    idx =    e1.outIdx
    e1.outIdx = e2.outIdx
    e2.outIdx = idx

def _InitEdge(e, eNext, ePrev, pt, polyType):
    e.nextE = eNext
    e.prevE = ePrev
    e.xCurr = pt.x
    e.yCurr = pt.y
    if e.yCurr >= e.nextE.yCurr:
        e.xBot = e.xCurr
        e.yBot = e.yCurr
        e.xTop = e.nextE.xCurr
        e.yTop = e.nextE.yCurr
        e.windDelta = 1
    else:
        e.xTop = e.xCurr
        e.yTop = e.yCurr
        e.xBot = e.nextE.xCurr
        e.yBot = e.nextE.yCurr
        e.windDelta = -1
    _SetDx(e)
    e.outIdx = -1
    e.PolyType = polyType

def _SwapX(e):
    e.xCurr = e.xTop
    e.xTop = e.xBot
    e.xBot = e.xCurr

class ClipperBase(object):
    _EdgeList            = []   # 2D array
    _LocalMinList    = None     # single-linked list of LocalMinima
    _CurrentLocMin = None

    def _InsertLocalMinima(self, lm):
        if self._LocalMinList is None:
            self._LocalMinList = lm
        elif lm.y >= self._LocalMinList.y:
            lm.nextLm = self._LocalMinList
            self._LocalMinList = lm
        else:
            tmp = self._LocalMinList
            while tmp.nextLm is not None and lm.y < tmp.nextLm.y:
                    tmp = tmp.nextLm
            lm.nextLm = tmp.nextLm
            tmp.nextLm = lm

    def _AddBoundsToLML(self, e):
        e.nextInLML = None
        e = e.nextE
        while True:
            if e.dx == horizontal:
                if (e.nextE.yTop < e.yTop) and (e.nextE.xBot > e.prevE.xBot): break
                if (e.xTop != e.prevE.xBot): _SwapX(e)
                e.nextInLML = e.prevE
            elif e.yBot == e.prevE.yBot: break
            else: e.nextInLML = e.prevE
            e = e.nextE
        lm = None
        if e.dx == horizontal:
            if (e.xBot != e.prevE.xBot): _SwapX(e)
            lm = LocalMinima(e.prevE.yBot, e.prevE, e)
        elif (e.dx < e.prevE.dx):
            lm = LocalMinima(e.prevE.yBot, e.prevE, e)
        else:
            lm = LocalMinima(e.prevE.yBot, e, e.prevE)
        lm.leftBound.side = EdgeSide.Left
        lm.rightBound.side = EdgeSide.Right
        self._InsertLocalMinima(lm)
        while True:
            if e.nextE.yTop == e.yTop and e.nextE.dx != horizontal: break
            e.nextInLML = e.nextE
            e = e.nextE
            if e.dx == horizontal and e.xBot != e.prevE.xTop: _SwapX(e)
        return e.nextE

    def _Reset(self):
        lm = self._LocalMinList
        if lm is not None: self._CurrentLocMin = lm
        while lm is not None:
            e = lm.leftBound
            while e is not None:
                e.xCurr    = e.xBot
                e.yCurr    = e.yBot
                e.side     = EdgeSide.Left
                e.outIdx = -1
                e = e.nextInLML
            e = lm.rightBound
            while e is not None:
                e.xCurr    = e.xBot
                e.yCurr    = e.yBot
                e.side     = EdgeSide.Right
                e.outIdx = -1
                e = e.nextInLML
            lm = lm.nextLm

    def AddPolygon(self, polygon, polyType):
        ln = len(polygon)
        if ln < 3: return False
        pg = polygon[:]
        j = 0
        # remove duplicate points and co-linear points
        for i in range(1, len(polygon)):
            if _PointsEqual(pg[j], polygon[i]): 
                continue
            elif (j > 0) and _SlopesEqual(pg[j-1], pg[j], polygon[i]):
                if _PointsEqual(pg[j-1], polygon[i]): j -= 1
            else: j += 1
            pg[j] = polygon[i]
        if (j < 2): return False
        # remove duplicate points and co-linear edges at the loop around
        # of the start and end coordinates ...
        ln = j +1
        while (ln > 2):
            if _PointsEqual(pg[j], pg[0]): j -= 1
            elif _PointsEqual(pg[0], pg[1]) or _SlopesEqual(pg[j], pg[0], pg[1]):
                pg[0] = pg[j]
                j -= 1
            elif _SlopesEqual(pg[j-1], pg[j], pg[0]): j -= 1
            elif _SlopesEqual(pg[0], pg[1], pg[2]):
                for i in range(2, j +1): pg[i-1] = pg[i]
                j -= 1
            else: break
            ln -= 1
        if ln < 3: return False
        edges = []
        for i in range(ln):
            edges.append(Edge())
        edges[0].xCurr = pg[0].x
        edges[0].yCurr = pg[0].y
        _InitEdge(edges[ln-1], edges[0], edges[ln-2], pg[ln-1], polyType)
        for i in range(ln-2, 0, -1):
            _InitEdge(edges[i], edges[i+1], edges[i-1], pg[i], polyType)
        _InitEdge(edges[0], edges[1], edges[ln-1], pg[0], polyType)
        e = edges[0]
        eHighest = e
        while True:
            e.xCurr = e.xBot
            e.yCurr = e.yBot
            if e.yTop < eHighest.yTop: eHighest = e
            e = e.nextE
            if e == edges[0]: break
        # make sure eHighest is positioned so the following loop works safely ...
        if eHighest.windDelta > 0: eHighest = eHighest.nextE
        if eHighest.dx == horizontal: eHighest = eHighest.nextE
        # finally insert each local minima ...
        e = eHighest
        while True:
            e = self._AddBoundsToLML(e)
            if e == eHighest: break
        self._EdgeList.append(edges)

    def AddPolygons(self, polygons, polyType):
        result = False
        for p in polygons:
            if self.AddPolygon(p, polyType): result = True
        return result

    def Clear(self):
        self._EdgeList = []
        self._LocalMinList    = None
        self._CurrentLocMin = None

    def _PopLocalMinima(self):
        if self._CurrentLocMin is not None:
            self._CurrentLocMin = self._CurrentLocMin.nextLm

#===============================================================================
# Clipper class (+ data structs & ancilliary functions)
#===============================================================================
def _IntersectPoint(edge1, edge2):
    if _SlopesEqual2(edge1, edge2): return Point(0,0), False
    if edge1.dx == 0:
        x = edge1.xBot
        if edge2.dx == horizontal:
            y = edge2.yBot
        else:
            b2 = edge2.yBot - Decimal(edge2.xBot)/edge2.dx
            y = round(Decimal(x)/edge2.dx + b2)
    elif edge2.dx == 0:
        x = edge2.xBot
        if edge1.dx == horizontal:
            y = edge1.yBot
        else:
            b1 = edge1.yBot - Decimal(edge1.xBot)/edge1.dx
            y = round(Decimal(x)/edge1.dx + b1)
    else:
        b1 = edge1.xBot - edge1.yBot * edge1.dx
        b2 = edge2.xBot - edge2.yBot * edge2.dx
        m    = Decimal(b2-b1)/(edge1.dx - edge2.dx)
        y    = round(m)
        if math.fabs(edge1.dx) < math.fabs(edge2.dx):
            x = round(edge1.dx * m + b1)
        else:
            x = round(edge2.dx * m + b2)
    if (y < edge1.yTop) or (y < edge2.yTop):
        if (edge1.yTop > edge2.yTop):
            return Point(edge1.xTop,edge1.yTop), _TopX(edge2, edge1.yTop) < edge1.xTop
        else:
            return Point(edge2.xTop,edge2.yTop), _TopX(edge1, edge2.yTop) > edge2.xTop
    else:
        return Point(x,y), True

def _TopX(e, currentY):
    if currentY == e.yTop: return e.xTop
    elif e.xTop == e.xBot: return e.xBot
    else: return e.xBot + round(e.dx *(currentY - e.yBot))

def _E2InsertsBeforeE1(e1,e2):
    if e2.xCurr == e1.xCurr:
        return e2.dx > e1.dx
    else:
        return e2.xCurr < e1.xCurr

def _IsMinima(e):
    return e is not None and e.prevE.nextInLML != e and e.nextE.nextInLML != e

def _IsMaxima(e, y):
    return e is not None and e.yTop == y and e.nextInLML is None

def _IsIntermediate(e, y):
    return e.yTop == y and e.nextInLML is not None

def _GetMaximaPair(e):
    if not _IsMaxima(e.nextE, e.yTop) or e.nextE.xTop != e.xTop:
        return e.prevE
    else:
        return e.nextE

def _GetnextInAEL(e, direction):
    if direction == Direction.LeftToRight: return e.nextInAEL
    else: return e.prevInAEL

def _ProtectLeft(val):
    if val: return Protects.Both
    else: return Protects.Right

def _ProtectRight(val):
    if val: return Protects.Both
    else: return Protects.Left

def _SwapIntersectNodes(int1, int2):
    e1 = int1.e1
    e2 = int1.e2
    p = int1.pt
    int1.e1 = int2.e1
    int1.e2 = int2.e2
    int1.pt = int2.pt
    int2.e1 = e1
    int2.e2 = e2
    int2.pt = p

def _GetDx(pt1, pt2):
    if (pt1.y == pt2.y): return horizontal
    else: return Decimal(pt2.x - pt1.x)/(pt2.y - pt1.y)

def _ProcessParam1BeforeParam2(node1, node2):
    if node1.pt.y != node2.pt.y:
        return node1.pt.y > node2.pt.y
    if node1.e1 == node2.e1 or node1.e2 == node2.e1:
        result = node2.pt.x > node1.pt.x
        if node2.e1.dx > 0: result = not result
        return result
    elif node1.e1 == node2.e2 or node1.e2 == node2.e2:
        result = node2.pt.x > node1.pt.x
        if node2.e2.dx > 0: result = not result
        return result
    else:
        return node2.pt.x > node1.pt.x

def _Param1RightOfParam2(outRec1, outRec2):
    while outRec1 is not None:
        outRec1 = outRec1.FirstLeft
        if outRec1 == outRec2: return True
    return False

def _FirstParamIsbottomPt(btmPt1, btmPt2):
    p = btmPt1.prevOp
    while _PointsEqual(p.pt, btmPt1.pt) and (p != btmPt1): p = p.prevOp
    dx1p = abs(_GetDx(btmPt1.pt, p.pt))
    p = btmPt1.nextOp
    while _PointsEqual(p.pt, btmPt1.pt) and (p != btmPt1): p = p.nextOp
    dx1n = abs(_GetDx(btmPt1.pt, p.pt))

    p = btmPt2.prevOp
    while _PointsEqual(p.pt, btmPt2.pt) and (p != btmPt2): p = p.prevOp
    dx2p = abs(_GetDx(btmPt2.pt, p.pt))
    p = btmPt2.nextOp
    while _PointsEqual(p.pt, btmPt2.pt) and (p != btmPt2): p = p.nextOp
    dx2n = abs(_GetDx(btmPt2.pt, p.pt))
    return (dx1p >= dx2p and dx1p >= dx2n) or (dx1n >= dx2p and dx1n >= dx2n)

def _GetbottomPt(pp):
    dups = None
    p = pp.nextOp
    while p != pp:
        if p.pt.y > pp.pt.y:
            pp = p
            dups = None
        elif p.pt.y == pp.pt.y and p.pt.x <= pp.pt.x:
            if p.pt.x < pp.pt.x:
                dups = None
                pp = p
            else:
                if p.nextOp != pp and p.prevOp != pp: dups = p
        p = p.nextOp
    if dups is not None:
        while dups != p:
            if not _FirstParamIsbottomPt(p, dups): pp = dups
            dups = dups.nextOp
            while not _PointsEqual(dups.pt, pp.pt): dups = dups.nextOp
    return pp

def _GetLowermostRec(outRec1, outRec2):
    outPt1 = outRec1.bottomPt
    outPt2 = outRec2.bottomPt
    if (outPt1.pt.y > outPt2.pt.y): return outRec1
    elif (outPt1.pt.y < outPt2.pt.y): return outRec2
    elif (outPt1.pt.x < outPt2.pt.x): return outRec1
    elif (outPt1.pt.x > outPt2.pt.x): return outRec2
    elif (outPt1.nextOp == outPt1): return outRec2
    elif (outPt2.nextOp == outPt2): return outRec1
    elif _FirstParamIsbottomPt(outPt1, outPt2): return outRec1
    else: return outRec2

def _SetHoleState(e, outRec, polyOutList):
    isHole = False
    e2 = e.prevInAEL
    while e2 is not None:
        if e2.outIdx >= 0:
            isHole = not isHole
            if outRec.FirstLeft is None:
                outRec.FirstLeft = polyOutList[e2.outIdx]
        e2 = e2.prevInAEL
    outRec.isHole = isHole

def _AddOutPt(e, pt, polyOutList):
    toFront = e.side == EdgeSide.Left
    if e.outIdx < 0:
        outRec = OutRec(len(polyOutList))
        e.outIdx = outRec.idx
        polyOutList.append(outRec)
        op = OutPt(outRec.idx, pt)
        op.nextOp = op
        op.prevOp = op
        outRec.pts = op
        outRec.bottomPt = op
        _SetHoleState(e, outRec, polyOutList)
    else:
        outRec = polyOutList[e.outIdx]
        op = outRec.pts
        if (toFront and _PointsEqual(pt, op.pt)) or \
            (not toFront and _PointsEqual(pt, op.prevOp.pt)): return
        op2 = OutPt(outRec.idx, pt)
        if (op2.pt.y == outRec.bottomPt.pt.y) and \
            (op2.pt.x < outRec.bottomPt.pt.x):
                outRec.bottomPt = op2
        op2.nextOp = op
        op2.prevOp = op.prevOp
        op.prevOp.nextOp = op2
        op.prevOp = op2
        if toFront: outRec.pts = op2

def _PointCount(pts):
    if pts is None: return 0
    p = pts
    result = 0
    while True:
        result += 1
        p = p.nextOp
        if p == pts: break
    return result

def _ReversePolyPtLinks(pp):
    if pp is None: return
    pp1 = pp
    while True:
        pp2 = pp1.nextOp
        pp1.nextOp = pp1.prevOp
        pp1.prevOp = pp2;
        pp1 = pp2
        if pp1 == pp: break

def _DoEdge1(e1, e2, pt, polyOutList):
    _AddOutPt(e1, pt, polyOutList)
    _SwapSides(e1, e2)
    _SwapPolyIndexes(e1, e2)

def _DoEdge2(e1, e2, pt, polyOutList):
    _AddOutPt(e2, pt, polyOutList)
    _SwapSides(e1, e2)
    _SwapPolyIndexes(e1, e2)

def _DoBothEdges(e1, e2, pt, polyOutList):
    _AddOutPt(e1, pt, polyOutList)
    _AddOutPt(e2, pt, polyOutList)
    _SwapSides(e1, e2)
    _SwapPolyIndexes(e1, e2)

def _FixupOutPolygon(outRec):
    lastOK = None
    outRec.pts = outRec.bottomPt
    pp = outRec.pts
    while True:
        if pp.prevOp == pp or pp.nextOp == pp.prevOp:
            outRec.pts = None
            outRec.bottomPt = None
            return

        if _PointsEqual(pp.pt, pp.nextOp.pt) or \
                _SlopesEqual(pp.prevOp.pt, pp.pt, pp.nextOp.pt):
            lastOK = None
            if pp == outRec.bottomPt:
                outRec.bottomPt = None
            pp.prevOp.nextOp = pp.nextOp
            pp.nextOp.prevOp = pp.prevOp
            pp = pp.prevOp
        elif pp == lastOK: break
        else:
            if lastOK is None: lastOK = pp
            pp = pp.nextOp
    if outRec.bottomPt is None:
        outRec.bottomPt = _GetbottomPt(pp)
        outRec.bottomPt.idx = outRec.idx
        outRec.pts = outRec.bottomPt

def _FixHoleLinkage(outRec):
    if outRec.FirstLeft is None or \
        (outRec.isHole != outRec.firstLeft.isHole and \
            outRec.firstLeft.pts is not None): return
    orfl = outRec.firstLeft
    while orfl is not None and \
            (orfl.isHole == outRec.isHole or orfl.pts is None):
        orfl = orfl.firstLeft
    outRec.firstLeft = orfl

class Clipper(ClipperBase):
    _PolyOutList        = []
    _ClipType             = ClipType.Intersection
    _Scanbeam             = None
    _ActiveEdges        = None
    _SortedEdges        = None
    _IntersectNodes = None
    _ClipFillType     = PolyFillType.EvenOdd
    _SubjFillType     = PolyFillType.EvenOdd
    _ExecuteLocked    = False
    _ReverseOutput    = False
    _UsingPolyTree    = False

    def _Reset(self):
        ClipperBase._Reset(self)
        _Scanbeam = None
        _PolyOutList = []
        lm = self._LocalMinList
        while lm is not None:
            self._InsertScanbeam(lm.y)
            self._InsertScanbeam(lm.leftBound.yTop)
            lm = lm.nextLm

    def Clear(self):
        _PolyOutList = []
        ClipperBase.Clear(self)

    def _InsertScanbeam(self, y):
        if self._Scanbeam is None:
            self._Scanbeam = Scanbeam(y)
        elif y > self._Scanbeam.y:
            self._Scanbeam = Scanbeam(y, self._Scanbeam)
        else:
            sb = self._Scanbeam
            while sb.nextSb is not None and y <= sb.nextSb.y:
                sb = sb.nextSb
            if y == sb.y: return
            newSb = Scanbeam(y, sb.nextSb)
            sb.nextSb = newSb

    def _PopScanbeam(self):
        result = self._Scanbeam.y
        self._Scanbeam = self._Scanbeam.nextSb
        return result

    def _SetWindingCount(self, edge):
        e = edge.prevInAEL
        while e is not None and e.PolyType != edge.PolyType:
            e = e.prevInAEL
        if e is None:
            edge.windCnt = edge.windDelta
            edge.windCnt2 = 0
            e = self._ActiveEdges
        elif self._IsEvenOddFillType(edge):
            edge.windCnt = 1
            edge.windCnt2 = e.windCnt2
            e = e.nextInAEL
        else:
            if e.windCnt * e.windDelta < 0:
                if (abs(e.windCnt) > 1):
                    if (e.windDelta * edge.windDelta < 0): edge.windCnt = e.windCnt
                    else: edge.windCnt = e.windCnt + edge.windDelta
                else:
                    edge.windCnt = e.windCnt + e.windDelta + edge.windDelta
            elif (abs(e.windCnt) > 1) and (e.windDelta * edge.windDelta < 0):
                edge.windCnt = e.windCnt
            elif e.windCnt + edge.windDelta == 0:
                edge.windCnt = e.windCnt
            else:
                edge.windCnt = e.windCnt + edge.windDelta
            edge.windCnt2 = e.windCnt2
            e = e.nextInAEL
        # update windCnt2 ...
        if self._IsEvenOddAltFillType(edge):
            while (e != edge):
                if edge.windCnt2 == 0: edge.windCnt2 = 1
                else: edge.windCnt2 = 0
                e = e.nextInAEL
        else:
            while (e != edge):
                edge.windCnt2 += e.windDelta
                e = e.nextInAEL

    def _IsEvenOddFillType(self, edge):
        if edge.PolyType == PolyType.Subject:
            return self._SubjFillType == PolyFillType.EvenOdd
        else:
            return self._ClipFillType == PolyFillType.EvenOdd

    def _IsEvenOddAltFillType(self, edge):
        if edge.PolyType == PolyType.Subject:
            return self._ClipFillType == PolyFillType.EvenOdd
        else:
            return self._SubjFillType == PolyFillType.EvenOdd

    def _IsContributing(self, edge):
        pft = pft2 = PolyType.Subject
        result = True
        if edge.PolyType == PolyType.Subject:
            pft = self._SubjFillType
            pft2 = self._ClipFillType
        else:
            pft = self._ClipFillType
            pft2 = self._SubjFillType
        if pft == PolyFillType.EvenOdd or pft == PolyFillType.NonZero:
            result = abs(edge.windCnt) == 1
        elif pft == PolyFillType.Positive:
            result = edge.windCnt == 1
        else: result = edge.windCnt == -1
        if not result: return False

        if self._ClipType == ClipType.Intersection: ###########
            if pft2 == PolyFillType.EvenOdd or pft2 == PolyFillType.NonZero:
                return edge.windCnt2 != 0
            elif pft2 == PolyFillType.Positive:
                return edge.windCnt2 > 0
            else:
                return edge.windCnt2 < 0 # Negative
        elif self._ClipType == ClipType.Union:            ###########
            if pft2 == PolyFillType.EvenOdd or pft2 == PolyFillType.NonZero:
                return edge.windCnt2 == 0
            elif pft2 == PolyFillType.Positive:
                return edge.windCnt2 <= 0
            else: return edge.windCnt2 >= 0 # Negative
        elif self._ClipType == ClipType.Difference: ###########
            if edge.PolyType == PolyType.Subject:
                if pft2 == PolyFillType.EvenOdd or pft2 == PolyFillType.NonZero:
                    return edge.windCnt2 == 0
                elif edge.PolyType == PolyFillType.Positive:
                    return edge.windCnt2 <= 0
                else:
                    return edge.windCnt2 >= 0
            else:
                if pft2 == PolyFillType.EvenOdd or pft2 == PolyFillType.NonZero:
                    return edge.windCnt2 != 0
                elif pft2 == PolyFillType.Positive:
                    return edge.windCnt2 > 0
                else:
                    return edge.windCnt2 < 0
        else: # self._ClipType == ClipType.XOR: ###########
            if pft2 == PolyFillType.EvenOdd or pft2 == PolyFillType.NonZero:
                return edge.windCnt2 != 0
            elif pft2 == PolyFillType.Positive:
                return edge.windCnt2 > 0
            else:
                return edge.windCnt2 < 0

    def _AddEdgeToSEL(self, edge):
        if self._SortedEdges is None:
            self._SortedEdges = edge
            edge.prevInSEL = None
            edge.nextInSEL = None
        else:
            # add edge to front of list ...
            edge.nextInSEL = self._SortedEdges
            edge.prevInSEL = None
            self._SortedEdges.prevInSEL = edge
            self._SortedEdges = edge

    def _CopyAELToSEL(self):
        e = self._ActiveEdges
        self._SortedEdges = e
        while e is not None:
            e.prevInSEL = e.prevInAEL
            e.nextInSEL = e.nextInAEL
            e = e.nextInAEL

    def _InsertEdgeIntoAEL(self, edge):
        edge.prevInAEL = None
        edge.nextInAEL = None
        if self._ActiveEdges is None:
            self._ActiveEdges = edge
        elif _E2InsertsBeforeE1(self._ActiveEdges, edge):
            edge.nextInAEL = self._ActiveEdges
            self._ActiveEdges.prevInAEL = edge
            self._ActiveEdges = edge
        else:
            e = self._ActiveEdges
            while e.nextInAEL is not None and \
                not _E2InsertsBeforeE1(e.nextInAEL, edge):
                    e = e.nextInAEL
            edge.nextInAEL = e.nextInAEL
            if e.nextInAEL is not None: e.nextInAEL.prevInAEL = edge
            edge.prevInAEL = e
            e.nextInAEL = edge

    def _InsertLocalMinimaIntoAEL(self, botY):
        while self._CurrentLocMin is not None and \
                 self._CurrentLocMin.y == botY:
            lb = self._CurrentLocMin.leftBound
            rb = self._CurrentLocMin.rightBound
            self._InsertEdgeIntoAEL(lb)
            self._InsertScanbeam(lb.yTop)
            self._InsertEdgeIntoAEL(rb)
            if self._IsEvenOddFillType(lb):
                lb.windDelta = 1
                rb.windDelta = 1
            else:
                rb.windDelta = -lb.windDelta
            self._SetWindingCount(lb)
            rb.windCnt = lb.windCnt
            rb.windCnt2 = lb.windCnt2
            if rb.dx == horizontal:
                self._AddEdgeToSEL(rb)
                self._InsertScanbeam(rb.nextInLML.yTop)
            else:
                self._InsertScanbeam(rb.yTop)
            if self._IsContributing(lb):
                self._AddLocalMinPoly(lb, rb, Point(lb.xCurr, self._CurrentLocMin.y))
            if (lb.nextInAEL != rb):
                e = lb.nextInAEL
                pt = Point(lb.xCurr, lb.yCurr)
                while e != rb:
                    self._IntersectEdges(rb, e, pt)
                    e = e.nextInAEL
            self._PopLocalMinima()

    def _SwapPositionsInAEL(self, e1, e2):
        if e1.nextInAEL == e2:
            nextE = e2.nextInAEL
            if nextE is not None: nextE.prevInAEL = e1
            prevE = e1.prevInAEL
            if prevE is not None: prevE.nextInAEL = e2
            e2.prevInAEL = prevE
            e2.nextInAEL = e1
            e1.prevInAEL = e2
            e1.nextInAEL = nextE
        elif e2.nextInAEL == e1:
            nextE = e1.nextInAEL
            if nextE is not None: nextE.prevInAEL = e2
            prevE = e2.prevInAEL
            if prevE is not None: prevE.nextInAEL = e1
            e1.prevInAEL = prevE
            e1.nextInAEL = e2
            e2.prevInAEL = e1
            e2.nextInAEL = nextE
        else:
            nextE = e1.nextInAEL
            prevE = e1.prevInAEL
            e1.nextInAEL = e2.nextInAEL
            if e1.nextInAEL is not None: e1.nextInAEL.prevInAEL = e1
            e1.prevInAEL = e2.prevInAEL
            if e1.prevInAEL is not None: e1.prevInAEL.nextInAEL = e1
            e2.nextInAEL = nextE
            if e2.nextInAEL is not None: e2.nextInAEL.prevInAEL = e2
            e2.prevInAEL = prevE
            if e2.prevInAEL is not None: e2.prevInAEL.nextInAEL = e2
        if e1.prevInAEL is None: self._ActiveEdges = e1
        elif e2.prevInAEL is None: self._ActiveEdges = e2

    def _SwapPositionsInSEL(self, e1, e2):
        if e1.nextInSEL == e2:
            nextE = e2.nextInSEL
            if nextE is not None: nextE.prevInSEL = e1
            prevE = e1.prevInSEL
            if prevE is not None: prevE.nextInSEL = e2
            e2.prevInSEL = prevE
            e2.nextInSEL = e1
            e1.prevInSEL = e2
            e1.nextInSEL = nextE
        elif e2.nextInSEL == e1:
            nextE = e1.nextInSEL
            if nextE is not None: nextE.prevInSEL = e2
            prevE = e2.prevInSEL
            if prevE is not None: prevE.nextInSEL = e1
            e1.prevInSEL = prevE
            e1.nextInSEL = e2
            e2.prevInSEL = e1
            e2.nextInSEL = nextE
        else:
            nextE = e1.nextInSEL
            prevE = e1.prevInSEL
            e1.nextInSEL = e2.nextInSEL
            e1.nextInSEL = e2.nextInSEL
            if e1.nextInSEL is not None: e1.nextInSEL.prevInSEL = e1
            e1.prevInSEL = e2.prevInSEL
            if e1.prevInSEL is not None: e1.prevInSEL.nextInSEL = e1
            e2.nextInSEL = nextE
            if e2.nextInSEL is not None: e2.nextInSEL.prevInSEL = e2
            e2.prevInSEL = prevE
            if e2.prevInSEL is not None: e2.prevInSEL.nextInSEL = e2
        if e1.prevInSEL is None: self._SortedEdges = e1
        elif e2.prevInSEL is None: self._SortedEdges = e2

    def _IsTopHorz(self, xPos):
        e = self._SortedEdges
        while e is not None:
            if (xPos >= min(e.xCurr,e.xTop)) and (xPos <= max(e.xCurr,e.xTop)):
                return False
            e = e.nextInSEL
        return True

    def _ProcessHorizontal(self, horzEdge):
        horzLeft = horzRight = 0
        direction = None
        if horzEdge.xCurr < horzEdge.xTop:
            horzLeft = horzEdge.xCurr
            horzRight = horzEdge.xTop
            direction = Direction.LeftToRight
        else:
            horzLeft = horzEdge.xTop
            horzRight = horzEdge.xCurr
            direction = Direction.RightToLeft
        eMaxPair = None
        if horzEdge.nextInLML is None:
            eMaxPair = _GetMaximaPair(horzEdge)
        e = _GetnextInAEL(horzEdge, direction)
        while e is not None:
            eNext = _GetnextInAEL(e, direction)
            if eMaxPair is not None or \
                ((direction == Direction.LeftToRight) and (e.xCurr <= horzRight)) or \
                ((direction == Direction.RightToLeft) and (e.xCurr >= horzLeft)):
                if (e.xCurr == horzEdge.xTop) and eMaxPair is None:
                    if _SlopesEqual2(e, horzEdge.nextInLML): break
                    elif e.dx < horzEdge.nextInLML.dx: break
                if e == eMaxPair:
                    if direction == Direction.LeftToRight:
                        self._IntersectEdges(horzEdge, e, Point(e.xCurr, horzEdge.yCurr))
                    else:
                        self._IntersectEdges(e, horzEdge, Point(e.xCurr, horzEdge.yCurr))
                    return
                elif e.dx == horizontal and not _IsMinima(e) and e.xCurr <= e.xTop:
                    if direction == Direction.LeftToRight:
                        self._IntersectEdges(horzEdge, e, Point(e.xCurr, horzEdge.yCurr),
                            _ProtectRight(not self._IsTopHorz(e.xCurr)))
                    else:
                        self._IntersectEdges(e, horzEdge, Point(e.xCurr, horzEdge.yCurr),
                            _ProtectLeft(not self._IsTopHorz(e.xCurr)))
                elif (direction == Direction.LeftToRight):
                    self._IntersectEdges(horzEdge, e, Point(e.xCurr, horzEdge.yCurr),
                        _ProtectRight(not self._IsTopHorz(e.xCurr)))
                else:
                    self._IntersectEdges(e, horzEdge, Point(e.xCurr, horzEdge.yCurr),
                        _ProtectLeft(not self._IsTopHorz(e.xCurr)))
                self._SwapPositionsInAEL(horzEdge, e)
            elif (self._SortedEdges is not None) and \
                ((direction == Direction.LeftToRight and e.xCurr > horzRight) or \
                (direction == Direction.RightToLeft and e.xCurr < horzLeft)): break
            e = eNext
        if horzEdge.nextInLML is not None:
            if horzEdge.outIdx >= 0:
                _AddOutPt(horzEdge, Point(horzEdge.xTop, horzEdge.yTop), self._PolyOutList)
            self._UpdateEdgeIntoAEL(horzEdge)
        else:
            if horzEdge.outIdx >= 0:
                self._IntersectEdges(horzEdge, eMaxPair, \
                    Point(horzEdge.xTop, horzEdge.yCurr), Protects.Both)
            if eMaxPair.outIdx >= 0: raise Exception("Clipper: Horizontal Error")
            self._DeleteFromAEL(eMaxPair)
            self._DeleteFromAEL(horzEdge)

    def _ProcessHorizontals(self):
        while self._SortedEdges is not None:
            e = self._SortedEdges
            self._DeleteFromSEL(e)
            self._ProcessHorizontal(e)

    def _AddIntersectNode(self, e1, e2, pt):
        newNode = IntersectNode(e1, e2, pt)
        if self._IntersectNodes is None:
            self._IntersectNodes = newNode
        elif _ProcessParam1BeforeParam2(newNode, self._IntersectNodes):
            newNode.nextIn = self._IntersectNodes
            self._IntersectNodes = newNode
        else:
            node = self._IntersectNodes
            while node.nextIn is not None and \
                _ProcessParam1BeforeParam2(node.nextIn, newNode):
                node = node.nextIn
            newNode.nextIn = node.nextIn
            node.nextIn = newNode

    def _ProcessIntersections(self, botY, topY):
        self._BuildIntersectList(botY, topY)
        if self._IntersectNodes is None: return True
        elif not self._FixupIntersections(): return False
        self._ProcessIntersectList()
        self._IntersectNodes = None
        return True

    def _BuildIntersectList(self, botY, topY):
        if self._ActiveEdges is None: return

        e = self._ActiveEdges
        self._SortedEdges = e
        while e is not None:
            e.prevInSEL = e.prevInAEL
            e.nextInSEL = e.nextInAEL
            e.tmpX = _TopX(e, topY)
            e = e.nextInAEL
        try:
            isModified = True
            while isModified and self._SortedEdges is not None:
                isModified = False
                e = self._SortedEdges
                while e.nextInSEL is not None:
                    eNext = e.nextInSEL
                    if e.tmpX <= eNext.tmpX:
                        e = eNext
                        continue
                    pt, intersected = _IntersectPoint(e, eNext)
                    if not intersected:
                        e = eNext
                        continue
                    if pt.y > botY:
                        pt = Point(_TopX(e, pt.y), botY)
                    self._AddIntersectNode(e, eNext, pt)
                    self._SwapPositionsInSEL(e, eNext)
                    isModified = True
                if e.prevInSEL is not None:
                    e.prevInSEL.nextInSEL = None
                else:
                    break
        finally:
            self._SortedEdges = None

    def _ProcessIntersectList(self):
        while self._IntersectNodes is not None:
            node = self._IntersectNodes
            self._IntersectEdges(node.e1, node.e2, node.pt, Protects.Both)
            self._SwapPositionsInAEL(node.e1, node.e2)
            self._IntersectNodes = node.nextIn

    def _DeleteFromAEL(self, e):
        aelPrev = e.prevInAEL
        aelNext = e.nextInAEL
        if aelPrev is None and aelNext is None and e != self._ActiveEdges:
            return
        if aelPrev is not None:
            aelPrev.nextInAEL = aelNext
        else:
            self._ActiveEdges = aelNext
        if aelNext is not None:
            aelNext.prevInAEL = aelPrev
        e.nextInAEL = None
        e.prevInAEL = None

    def _DeleteFromSEL(self, e):
        SELPrev = e.prevInSEL
        SELNext = e.nextInSEL
        if SELPrev is None and SELNext is None and e != self._SortedEdges:
            return
        if SELPrev is not None:
            SELPrev.nextInSEL = SELNext
        else:
            self._SortedEdges = SELNext
        if SELNext is not None:
            SELNext.prevInSEL = SELPrev
        e.nextInSEL = None
        e.prevInSEL = None

    def _IntersectEdges(self, e1, e2, pt, protects = Protects.Neither):
        e1stops = protects & Protects.Left == 0 and \
                e1.nextInLML is None and \
                e1.xTop == pt.x and e1.yTop == pt.y
        e2stops = protects & Protects.Right == 0 and \
                e2.nextInLML is None and \
                e2.xTop == pt.x and e2.yTop == pt.y
        e1Contributing = e1.outIdx >= 0
        e2contributing = e2.outIdx >= 0

        if e1.PolyType == e2.PolyType:
            if self._IsEvenOddFillType(e1):
                e1Wc = e1.windCnt
                e1.windCnt = e2.windCnt
                e2.windCnt = e1Wc
            else:
                if e1.windCnt + e2.windDelta == 0: e1.windCnt = -e1.windCnt
                else: e1.windCnt += e2.windDelta
                if e2.windCnt - e1.windDelta == 0: e2.windCnt = -e2.windCnt
                else: e2.windCnt -= e1.windDelta
        else:
            if not self._IsEvenOddFillType(e2): e1.windCnt2 += e2.windDelta
            elif e1.windCnt2 == 0: e1.windCnt2 = 1
            else: e1.windCnt2 = 0
            if not self._IsEvenOddFillType(e1): e2.windCnt2 -= e1.windDelta
            elif e2.windCnt2 == 0: e2.windCnt2 = 1
            else: e2.windCnt2 = 0

        e1FillType = e2FillType = PolyFillType.EvenOdd
        if e1.PolyType == PolyType.Subject:
            e1FillType = self._SubjFillType
            e1FillType2 = self._ClipFillType
        else:
            e1FillType = self._ClipFillType
            e1FillType2 = self._SubjFillType

        if e2.PolyType == PolyType.Subject:
            e2FillType = self._SubjFillType
            e2FillType2 = self._ClipFillType
        else:
            e2FillType = self._ClipFillType
            e2FillType2 = self._SubjFillType

        e1Wc = e2Wc = 0
        if e1FillType == PolyFillType.Positive: e1Wc = e1.windCnt
        elif e1FillType == PolyFillType.Negative: e1Wc = -e1.windCnt
        else: e1Wc = abs(e1.windCnt)

        if e2FillType == PolyFillType.Positive: e2Wc = e2.windCnt
        elif e2FillType == PolyFillType.Negative: e2Wc = -e2.windCnt
        else: e2Wc = abs(e2.windCnt)

        if e1Contributing and e2contributing:
            if e1stops or e2stops or abs(e1Wc) > 1 or abs(e2Wc) > 1 or \
                (e1.PolyType != e2.PolyType and self._ClipType != ClipType.Xor):
                    self._AddLocalMaxPoly(e1, e2, pt)
            else:
                _DoBothEdges(e1, e2, pt, self._PolyOutList)
        elif e1Contributing:
            if (e2Wc == 0 or e2Wc == 1) and \
                (self._ClipType != ClipType.Intersection or \
                e2.PolyType == PolyType.Subject or \
                e2.windCnt2 != 0): _DoEdge1(e1, e2, pt, self._PolyOutList)
        elif e2contributing:
            if (e1Wc == 0 or e1Wc == 1) and \
                (self._ClipType != ClipType.Intersection or \
                e1.PolyType == PolyType.Subject or \
                e1.windCnt2 != 0): _DoEdge2(e1, e2, pt, self._PolyOutList)

        elif    (e1Wc == 0 or e1Wc == 1) and (e2Wc == 0 or e2Wc == 1) and \
            not e1stops and not e2stops:

            e1Wc2 = e2Wc2 = 0
            e1FillType2 = e2FillType2 = PolyFillType.EvenOdd
            if e1FillType2 == PolyFillType.Positive: e1Wc2 = e1.windCnt2
            elif e1FillType2 == PolyFillType.Negative: e1Wc2 = -e1.windCnt2
            else: e1Wc2 = abs(e1.windCnt2)
            if e2FillType2 == PolyFillType.Positive: e2Wc2 = e2.windCnt2
            elif e2FillType2 == PolyFillType.Negative: e2Wc2 = -e2.windCnt2
            else: e2Wc2 = abs(e2.windCnt2)

            if e1.PolyType != e2.PolyType:
                self._AddLocalMinPoly(e1, e2, pt)
            elif e1Wc == 1 and e2Wc == 1:
                if self._ClipType == ClipType.Intersection:
                    if e1Wc2 > 0 and e2Wc2 > 0:
                        self._AddLocalMinPoly(e1, e2, pt)
                elif self._ClipType == ClipType.Union:
                    if e1Wc2 <= 0 and e2Wc2 <= 0:
                        self._AddLocalMinPoly(e1, e2, pt)
                elif self._ClipType == ClipType.Difference:
                    if (e1.PolyType == PolyType.Clip and e1Wc2 > 0 and e2Wc2 > 0) or \
                        (e1.PolyType == PolyType.Subject and e1Wc2 <= 0 and e2Wc2 <= 0):
                            self._AddLocalMinPoly(e1, e2, pt)
                else:
                    self._AddLocalMinPoly(e1, e2, pt)
            else:
                _SwapSides(e1, e2, self._PolyOutList)

        if e1stops != e2stops and \
            ((e1stops and e1.outIdx >= 0) or (e2stops and e2.outIdx >= 0)):
                _SwapSides(e1, e2, self._PolyOutList)
                _SwapPolyIndexes(e1, e2)
        if e1stops: self._DeleteFromAEL(e1)
        if e2stops: self._DeleteFromAEL(e2)

    def _DoMaxima(self, e, topY):
        eMaxPair = _GetMaximaPair(e)
        x = e.xTop
        eNext = e.nextInAEL
        while eNext != eMaxPair:
            if eNext is None: raise Exception("DoMaxima error")
            self._IntersectEdges(e, eNext, Point(x, topY), Protects.Both)
            self._SwapPositionsInAEL(e, eNext)
            eNext = eNext.nextInAEL
        if e.outIdx < 0 and eMaxPair.outIdx < 0:
            self._DeleteFromAEL(e)
            self._DeleteFromAEL(eMaxPair)
        elif e.outIdx >= 0 and eMaxPair.outIdx >= 0:
            self._IntersectEdges(e, eMaxPair, Point(x, topY))
        else:
            raise Exception("DoMaxima error")

    def _UpdateEdgeIntoAEL(self, e):
        if e.nextInLML is None:
            raise Exception("UpdateEdgeIntoAEL error")
        aelPrev = e.prevInAEL
        aelNext = e.nextInAEL
        e.nextInLML.outIdx = e.outIdx
        if aelPrev is not None:
            aelPrev.nextInAEL = e.nextInLML
        else:
            self._ActiveEdges = e.nextInLML
        if aelNext is not None:
            aelNext.prevInAEL = e.nextInLML
        e.nextInLML.side = e.side
        e.nextInLML.windDelta = e.windDelta
        e.nextInLML.windCnt = e.windCnt
        e.nextInLML.windCnt2 = e.windCnt2
        e = e.nextInLML
        e.prevInAEL = aelPrev
        e.nextInAEL = aelNext
        if e.dx != horizontal:
            self._InsertScanbeam(e.yTop)
        return e

    def _AddLocalMinPoly(self, e1, e2, pt):
        if e2.dx == horizontal or e1.dx > e2.dx:
            _AddOutPt(e1, pt, self._PolyOutList)
            e2.outIdx = e1.outIdx
            e1.side = EdgeSide.Left
            e2.side = EdgeSide.Right
        else:
            _AddOutPt(e2, pt, self._PolyOutList)
            e1.outIdx = e2.outIdx
            e1.side = EdgeSide.Right
            e2.side = EdgeSide.Left

    def _AddLocalMaxPoly(self, e1, e2, pt):
        _AddOutPt(e1, pt, self._PolyOutList)
        if e1.outIdx == e2.outIdx:
            e1.outIdx = -1
            e2.outIdx = -1
        elif e1.outIdx < e2.outIdx:
            self._AppendPolygon(e1, e2)
        else:
            self._AppendPolygon(e2, e1)

    def _AppendPolygon(self, e1, e2):
        outRec1 = self._PolyOutList[e1.outIdx]
        outRec2 = self._PolyOutList[e2.outIdx]
        holeStateRec = None
        if _Param1RightOfParam2(outRec1, outRec2): holeStateRec = outRec2
        elif _Param1RightOfParam2(outRec2, outRec1): holeStateRec = outRec1
        else: holeStateRec = _GetLowermostRec(outRec1, outRec2)
                
        p1_lft = outRec1.pts
        p2_lft = outRec2.pts
        p1_rt = p1_lft.prevOp
        p2_rt = p2_lft.prevOp
        newSide = EdgeSide.Left
        
        if e1.side == EdgeSide.Left:
            if e2.side == EdgeSide.Left:
                # z y x a b c
                _ReversePolyPtLinks(p2_lft)
                p2_lft.nextOp = p1_lft
                p1_lft.prevOp = p2_lft
                p1_rt.nextOp = p2_rt
                p2_rt.prevOp = p1_rt
                outRec1.pts = p2_rt
            else:
                # x y z a b c
                p2_rt.nextOp = p1_lft
                p1_lft.prevOp = p2_rt
                p2_lft.prevOp = p1_rt
                p1_rt.nextOp = p2_lft
                outRec1.pts = p2_lft
        else:
            newSide = EdgeSide.Right
            if e2.side == EdgeSide.Right:
                # a b c z y x
                _ReversePolyPtLinks(p2_lft)
                p1_rt.nextOp = p2_rt
                p2_rt.prevOp = p1_rt
                p2_lft.nextOp = p1_lft
                p1_lft.prevOp = p2_lft
            else:
                # a b c x y z
                p1_rt.nextOp = p2_lft
                p2_lft.prevOp = p1_rt
                p1_lft.prevOp = p2_rt
                p2_rt.nextOp = p1_lft
                
        if holeStateRec == outRec2:
            outRec1.bottomPt = outRec2.bottomPt
            outRec1.bottomPt.idx = outRec1.idx
            if outRec2.FirstLeft != outRec1:
                outRec1.FirstLeft = outRec2.FirstLeft
            outRec1.isHole = outRec2.isHole
        outRec2.pts = None
        outRec2.bottomPt = None
        outRec2.FirstLeft = outRec1
        OKIdx = outRec1.idx
        ObsoleteIdx = outRec2.idx

        e1.outIdx = -1
        e2.outIdx = -1

        e = self._ActiveEdges
        while e is not None:
            if e.outIdx == ObsoleteIdx:
                e.outIdx = OKIdx
                e.side = newSide
                break
            e = e.nextInAEL

    def _FixupIntersections(self):
        if self._IntersectNodes.nextIn is None: return True
        try:
            self._CopyAELToSEL()
            int1 = self._IntersectNodes
            int2 = self._IntersectNodes.nextIn
            e1 = e2 = None
            while int2 is not None:
                e1 = int1.e1
                if e1.prevInSEL == int1.e2: e2 = e1.prevInSEL
                elif (e1.nextInSEL == int1.e2): e2 = e1.nextInSEL
                else:
                    while int2 is not None:
                        if int2.e1.nextInSEL == int2.e2 or \
                            int2.e1.prevInSEL == int2.e2: break
                        int2 = int2.nextIn
                    if int2 is None:
                        self._SortedEdges = None
                        return False
                    _SwapIntersectNodes(int1, int2)
                    e1 = int1.e1
                    e2 = int1.e2
                self._SwapPositionsInSEL(e1, e2)
                int1 = int1.nextIn
                int2 = int1.nextIn
        finally:
            self._SortedEdges = None            
        return int1.e1.prevInSEL == int1.e2 or int1.e1.nextInSEL == int1.e2
            
    def _ProcessEdgesAtTopOfScanbeam(self, topY):
        e = self._ActiveEdges
        while e is not None:
            if _IsMaxima(e, topY) and _GetMaximaPair(e).dx != horizontal:
                ePrev = e.prevInAEL
                self._DoMaxima(e, topY)
                if ePrev is None: e = self._ActiveEdges
                else: e = ePrev.nextInAEL
            else:
                if _IsIntermediate(e, topY) and e.nextInLML.dx == horizontal:
                    if e.outIdx >= 0:
                        _AddOutPt(e, Point(e.xTop, e.yTop), self._PolyOutList)
                    e = self._UpdateEdgeIntoAEL(e)
                    self._AddEdgeToSEL(e)
                else:
                    e.xCurr = _TopX(e, topY)
                    e.yCurr = topY
                e = e.nextInAEL

        self._ProcessHorizontals()

        e = self._ActiveEdges
        while e is not None:
            if _IsIntermediate(e, topY):
                if (e.outIdx >= 0) :
                    _AddOutPt(e, Point(e.xTop, e.yTop), self._PolyOutList)
                e = self._UpdateEdgeIntoAEL(e)
            e = e.nextInAEL
            
    def ValidateAEL(self, val, s):
        e = self._ActiveEdges
        if e is None: return
        while e.nextInAEL is not None:
            if e.nextInAEL.prevInAEL is not e: 
                print(str(val) + ": " + s)
                raise Exception("oops!")  
            e = e.nextInAEL
            
    def DebugAEL(self, y):
        print(y)
        e = self._ActiveEdges
        while e is not None:
            print(e)
            e = e.nextInAEL
          
    def _Area(self, pts):
        # see http://www.mathopenref.com/coordpolygonarea2.html
        result = 0.0
        p = pts
        while True:
            result += (p.pt.x + p.prevOp.pt.x) * (p.prevOp.pt.y - p.pt.y)
            p = p.nextOp
            if p == pts: break
        return result / 2
        
    def _ExecuteInternal(self):
        #try:
            self._Reset()
            if self._Scanbeam is None: return True
            botY = self._PopScanbeam()
            while self._Scanbeam is not None:
                self._InsertLocalMinimaIntoAEL(botY)
                self._ProcessHorizontals()
                topY = self._PopScanbeam()
                # self.ValidateAEL(botY, "a")
                # self.DebugAEL(botY) ############# 
                if not self._ProcessIntersections(botY, topY): return False
                self._ProcessEdgesAtTopOfScanbeam(topY)
                botY = topY
                
            for outRec in self._PolyOutList:
                if outRec.pts is None: continue                
                _FixupOutPolygon(outRec)
                if outRec.pts is None: continue
                if outRec.isHole == (self._Area(outRec.pts) > 0.0):
                    _ReversePolyPtLinks(outRec.pts)
            return True
        #except:
        #    return False
    
    def Execute(
            self,
            clipType,
            solution,
            subjFillType = PolyFillType.EvenOdd,
            clipFillType = PolyFillType.EvenOdd):
        if self._ExecuteLocked: return False
        try:
            self._ExecuteLocked = True
            del solution[:]
            self._SubjFillType = subjFillType
            self._ClipFillType = clipFillType
            self._ClipType = clipType
            result = self._ExecuteInternal()
            if result: self._BuildResult(solution)
        finally:
            self._ExecuteLocked = False
        return result

    def _BuildResult(self, polygons):
        for outRec in self._PolyOutList:
            if outRec is None: 
                continue
            cnt = _PointCount(outRec.pts)
            if (cnt < 3): 
                continue
            poly = []
            op = outRec.pts
            for _ in range(cnt):
                poly.append(Point(op.pt.x, op.pt.y))
                op = op.prevOp
            polygons.append(poly)
        return
    
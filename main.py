from clipper import Area, Clipper, Point, ClipType, PolyType, PolyFillType
import math
import re
from random import randint

#===============================================================================
#===============================================================================

def HtmlColor(val):
    return "#{0:06x}".format(val & 0xFFFFFF)
#===============================================================================

def AlphaClr(val):
    return "{0:.2f}".format(float(val >> 24)/255)
#===============================================================================

def LoadFile1(lines):
    # File type 1: first line is total polygons count and subsequent lines 
    # contain the polygon vertex count followed by its coords 
    try:
        polygons = []
        poly = []
        for l in lines:
            vals = re.split(' |, |,', l.strip())
            if len(vals) < 2:  
                if (len(poly)  > 2):
                    polygons.append(poly)
                poly = []
            else: 
                poly.append(Point(int(vals[0]), int(vals[1])))
        if (len(poly)  > 2):
            polygons.append(poly)
        return polygons
    except:
        return None
#===============================================================================

def LoadFile2(lines):
    # File type 2: vertex coords on consecutive lines for each polygon 
    # where each polygon is separated by an empty line 
    try:
        polygons = []
        poly = []
        for l in lines:
            l = l.strip()
            if (l == ''): 
                if (len(poly)  > 2):
                    polygons.append(poly)
                poly = []
            else: 
                vals = re.split(' |, |,', l)
                poly.append(Point(int(vals[0]), int(vals[1])))
        if (len(poly)  > 2):
            polygons.append(poly)
        return polygons
    except:
        return None
#===============================================================================

def LoadFile(filename):
    try:
        f = open(filename, 'r')
        try:
            lines = f.readlines()
        finally:
            f.close()
        # pick file type from format of first line ...
        if len(lines) == 0: return []
        elif not ',' in lines[0]: return LoadFile1(lines)
        else: return LoadFile2(lines)
    except:
        return None
#===============================================================================
    
def SaveToFile(filename, polys, decimals = 0):
    scaling = math.pow(10, decimals)
    try:
        f = open(filename, 'w')
        try:
            if scaling == 1:
                for poly in polys:
                    for pt in poly:
                        f.write("{0}, {1}\n".format(pt.x, pt.y))
                    f.write("\n")
            else:
                for poly in polys:
                    for pt in poly:
                        f.write("{0:.4f}, {1:.4f}\n".format(pt.x / scaling, pt.y / scaling))
                    f.write("\n")
        finally:
            f.close()
    except:
        return

#===============================================================================
# Generate random polygon
#===============================================================================
    
def RandomPoly(maxWidth, maxHeight, vertCnt):
    result = []
    for _ in range(vertCnt):
        result.append(Point(randint(0, maxWidth), randint(0, maxHeight)))
    return result

#===============================================================================
# SVGBuilder
#===============================================================================
class SVGBuilder(object):
    
    class StyleInfo(object):
        fillType = PolyFillType.EvenOdd
        brushClr = 0
        penClr = 0
        penWidth = 0.8
        showCoords = False
    
    class StyleInfoPlus(StyleInfo):
        polygons  = []
        
    GlobalStyle = StyleInfo()
    PolyInfoList = []
    Header = """<?xml version=\"1.0\" standalone=\"no\"?> 
<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.0//EN\" 
\"http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd\"> 
\n<svg width=\"{0}px\" height=\"{1}px\" viewBox=\"0 0 {0} {1}\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">
  <defs>
    <filter id="Gamma">
      <feComponentTransfer>
        <feFuncR type="gamma" amplitude="1" exponent="0.3" offset="0" />
        <feFuncG type="gamma" amplitude="1" exponent="0.3" offset="0" />
        <feFuncB type="gamma" amplitude="1" exponent="0.3" offset="0" />
      </feComponentTransfer>
    </filter>
  </defs>\n\n"""
    PathFormat = "\"\n style=\"fill:{0}; fill-opacity:{1}; fill-rule:{2}; stroke:{3}; stroke-opacity:{4}; stroke-width:{5:.2f};\" filter=\"url(#Gamma)\"/>\n\n"
    
    def AddPolygon(self, poly, brushColor, penColor):
        if poly is None or len(poly) == 0: return
        pi = self.StyleInfoPlus()
        pi.penWidth = self.GlobalStyle.penWidth
        pi.fillType = self.GlobalStyle.fillType
        pi.showCoords = self.GlobalStyle.showCoords
        pi.brushClr = brushColor
        pi.penClr = penColor        
        pi.polygons.append(poly)
        self.PolyInfoList.Append(pi)
    
    def AddPolygons(self, polys, brushColor, penColor):
        if polys is None or len(polys) == 0: return
        pi = self.StyleInfoPlus()
        pi.penWidth = self.GlobalStyle.penWidth
        pi.fillType = self.GlobalStyle.fillType
        pi.showCoords = self.GlobalStyle.showCoords
        pi.brushClr = brushColor
        pi.penClr = penColor        
        pi.polygons = polys
        self.PolyInfoList.append(pi)
    
    def SaveToFile(self, filename, scale = 1.0, margin = 10):
        if len(self.PolyInfoList) == 0: return False
        if scale == 0: scale = 1.0
        if margin < 0: margin = 0
        pi = self.PolyInfoList[0]
        # get bounding rect ...
        left = right = pi.polygons[0][0].x
        top = bottom = pi.polygons[0][0].y
        for pi in self.PolyInfoList:
            for p in pi.polygons:
                for ip in p:
                    if ip.x < left: left = ip.x
                    if ip.x > right: right = ip.x
                    if ip.y < top: top = ip.y
                    if ip.y > bottom: bottom = ip.y
        left *= scale
        top *= scale
        right *= scale
        bottom *= scale
        offsetX = -left + margin      
        offsetY = -top + margin      
                    
        f = open(filename, 'w')
        m2 = margin * 2
        f.write(self.Header.format(right - left + m2, bottom - top + m2))
        for pi in self.PolyInfoList:
            f.write(" <path d=\"")
            for p in pi.polygons:
                cnt = len(p)
                if cnt < 3: continue
                f.write(" M {0:.2f} {1:.2f}".format(p[0].x * scale + offsetX, p[0].y * scale + offsetY))
                for i in range(1,cnt):
                    f.write(" L {0:.2f} {1:.2f}".format(p[i].x * scale + offsetX, p[i].y * scale + offsetY))
                f.write(" z")
            fillRule = "evenodd"
            if pi.fillType != PolyFillType.EvenOdd: fillRule = "nonzero"
            f.write(self.PathFormat.format(HtmlColor(pi.brushClr), 
                AlphaClr(pi.brushClr), fillRule, HtmlColor(pi.penClr), AlphaClr(pi.penClr),  pi.penWidth))
            
            if (pi.showCoords):
                f.write("<g font-family=\"Verdana\" font-size=\"11\" fill=\"black\">\n\n")
                for p in pi.polygons:
                    cnt = len(p)
                    if cnt < 3: continue
                    for pt in p:
                        x = pt.x * scale + offsetX
                        y = pt.y * scale + offsetY
                        f.write("<text x=\"{0}\" y=\"{1}\">{2},{3}</text>\n".format(x, y, pt.x, pt.y))
                    f.write("\n")
                f.write("</g>\n")
    
        f.write("</svg>\n")
        f.close()
        return True

#===============================================================================
#===============================================================================

#subj = LoadFile('./subj.txt')
#clip = LoadFile('./clip.txt')

subj = []
subj.append(RandomPoly(640, 480, 50))
clip = []
clip.append(RandomPoly(640, 480, 50))

c = Clipper()
c.AddPolygons(subj, PolyType.Subject)
c.AddPolygons(clip, PolyType.Clip)

solution = [] 
pft = PolyFillType.NonZero
result = c.Execute(ClipType.Intersection, solution, pft, pft)

SaveToFile('./solution.txt', solution)

svgBuilder = SVGBuilder()
# svgBuilder.GlobalStyle.showCoords = True
svgBuilder.GlobalStyle.fillType = pft
svgBuilder.AddPolygons(subj, 0x402020FF, 0x802020FF)
svgBuilder.AddPolygons(clip, 0x40FFFF20, 0x80FF2020)
svgBuilder.GlobalStyle.showCoords = False
svgBuilder.AddPolygons(solution, 0x60138013, 0xFF003300)
holes = []
for poly in solution: 
    if Area(poly) < 0: holes.append(poly)
svgBuilder.GlobalStyle.penWidth = 0.6
svgBuilder.AddPolygons(holes, 0x0, 0xFFFF0000)
svgBuilder.SaveToFile('./test.svg', 1.0, 100)

if not result: print("failed")
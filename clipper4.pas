unit clipper4;

(*******************************************************************************
*                                                                              *
* Author    :  Angus Johnson                                                   *
* Version   :  4.0.5 (beta)                                                    *
* Date      :  15 March 2011                                                   *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2011                                         *
*                                                                              *
* License:                                                                     *
* Use, modification & distribution is subject to Boost Software License Ver 1. *
* http://www.boost.org/LICENSE_1_0.txt                                         *
*                                                                              *
* Attributions:                                                                *
* The code in this library is an extension of Bala Vatti's clipping algorithm: *
* "A generic solution to polygon clipping"                                     *
* Communications of the ACM, Vol 35, Issue 7 (July 1992) pp 56-63.             *
* http://portal.acm.org/citation.cfm?id=129906                                 *
*                                                                              *
* Computer graphics and geometric modeling: implementation and algorithms      *
* By Max K. Agoston                                                            *
* Springer; 1 edition (January 4, 2005)                                        *
* http://books.google.com/books?q=vatti+clipping+agoston                       *
*                                                                              *
*******************************************************************************)

interface

uses
  SysUtils, Types, Classes, Math;

type

  TClipType = (ctIntersection, ctUnion, ctDifference, ctXor);
  TPolyType = (ptSubject, ptClip);
  TPolyFillType = (pftEvenOdd, pftNonZero);

  //used internally ...
  TEdgeSide = (esLeft, esRight);
  TIntersectProtect = (ipLeft, ipRight);
  TIntersectProtects = set of TIntersectProtect;
  TDirection = (dRightToLeft, dLeftToRight);
  TArrayOfPoint = array of TPoint;
  TArrayOfArrayOfPoint = array of TArrayOfPoint;

  PEdge = ^TEdge;
  TEdge = record
    xbot : integer;  //bottom
    ybot : integer;
    xcurr: integer;  //current (ie relative to bottom of current scanbeam)
    ycurr: integer;
    xtop : integer;  //top
    ytop : integer;
    tmpX :  integer;
    dx   : double;   //the inverse of slope
    polyType : TPolyType;
    side     : TEdgeSide;
    windDelta: integer; //1 or -1 depending on winding direction
    windCnt  : integer;
    windCnt2 : integer;  //winding count of the opposite polytype
    outIdx   : integer;
    next     : PEdge;
    prev     : PEdge;
    nextInLML: PEdge;
    prevInAEL: PEdge;
    nextInAEL: PEdge;
    prevInSEL: PEdge;
    nextInSEL: PEdge;
  end;

  PEdgeArray = ^TEdgeArray;
  TEdgeArray = array[0.. MaxInt div sizeof(TEdge) -1] of TEdge;

  PScanbeam = ^TScanbeam;
  TScanbeam = record
    y   : integer;
    next: PScanbeam;
  end;

  PIntersectNode = ^TIntersectNode;
  TIntersectNode = record
    edge1: PEdge;
    edge2: PEdge;
    pt   : TPoint;
    next : PIntersectNode;
  end;

  PLocalMinima = ^TLocalMinima;
  TLocalMinima = record
    y         : integer;
    leftBound : PEdge;
    rightBound: PEdge;
    next      : PLocalMinima;
  end;

  PPolyPt = ^TPolyPt;
  TPolyPt = record
    pt    : TPoint;
    next  : PPolyPt;
    prev  : PPolyPt;
    isDone: boolean;
  end;

  PTracer = ^TTracer;
  TTracer = record
    dirF : boolean; //direction (forward = true)
    pp   : PPolyPt;
    next : PTracer;
    prev : PTracer;
  end;

  PHoleInfo = ^THoleInfo;
  THoleInfo = record
    pp       : PPolyPt;
    isHole   : boolean;
    idx      : integer;
    IsDone   : boolean;
  end;

  TClipperBase4 = class
  private
    fEdgeList      : TList;
    fLmList        : PLocalMinima; //localMinima list
    fCurrLm        : PLocalMinima; //current localMinima node
    procedure DisposeLocalMinimaList;
  protected
    function Reset: boolean; virtual;
    procedure PopLocalMinima;
    property CurrentLm: PLocalMinima read fCurrLm;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AddPolygon(const polygon: TArrayOfPoint; polyType: TPolyType);
    procedure AddPolygons(const polygons: TArrayOfArrayOfPoint; polyType: TPolyType);
    procedure Clear; virtual;
  end;

  TClipper4 = class(TClipperBase4)
  private
    fPolyPtList    : TList;
    fHoleInfoList  : TList;
    fClipType      : TClipType;
    fScanbeam      : PScanbeam; //scanbeam list
    fActiveEdges   : PEdge;     //active edge list
    fSortedEdges   : PEdge;     //used for temporary sorting
    fIntersectNodes: PIntersectNode;
    fExecuteLocked : boolean;
    fClipFillType  : TPolyFillType;
    fSubjFillType  : TPolyFillType;
    procedure DisposeScanbeamList;
    procedure InsertScanbeam(const y: integer);
    function PopScanbeam: integer;
    procedure SetWindingCount(edge: PEdge);
    function IsNonZeroFillType(edge: PEdge): boolean;
    function IsNonZeroAltFillType(edge: PEdge): boolean;
    procedure AddEdgeToSEL(edge: PEdge);
    procedure CopyAELToSEL;
    procedure InsertLocalMinimaIntoAEL(const botY: integer);
    procedure SwapPositionsInAEL(e1, e2: PEdge);
    procedure SwapPositionsInSEL(e1, e2: PEdge);
    function IsTopHorz(const XPos: integer): boolean;
    procedure ProcessHorizontal(horzEdge: PEdge);
    procedure ProcessHorizontals;
    procedure AddIntersectNode(e1, e2: PEdge; const pt: TPoint);
    function ProcessIntersections(const topY: integer): boolean;
    procedure BuildIntersectList(const topY: integer);
    procedure ProcessIntersectList;
    procedure DeleteFromAEL(e: PEdge);
    procedure DeleteFromSEL(e: PEdge);
    procedure IntersectEdges(e1,e2: PEdge;
      const pt: TPoint; protects: TIntersectProtects = []);
    procedure DoMaxima(e: PEdge; const topY: integer);
    procedure UpdateEdgeIntoAEL(var e: PEdge);
    function FixupIntersections: boolean;
    procedure SwapIntersectNodes(int1, int2: PIntersectNode);
    procedure ProcessEdgesAtTopOfScanbeam(const topY: integer);
    function IsContributing(edge: PEdge): boolean;
    function AddPolyPt(e: PEdge; const pt: TPoint): PPolyPt;
    procedure AddLocalMaxPoly(e1, e2: PEdge; const pt: TPoint);
    procedure AddLocalMinPoly(e1, e2: PEdge; const pt: TPoint);
    procedure AppendPolygon(e1, e2: PEdge);
    procedure DisposeAllPolyPts;
    procedure DisposeIntersectNodes;
    function GetResult: TArrayOfArrayOfPoint;
    function FixupOutPolygon(outPoly: PPolyPt): PPolyPt;
    procedure GetHoleStates;
  protected
    function Reset: boolean; override;
  public
    function Execute(clipType: TClipType;
      out solution: TArrayOfArrayOfPoint;
      subjFillType: TPolyFillType = pftEvenOdd;
      clipFillType: TPolyFillType = pftEvenOdd): boolean;
    constructor Create; override;
    destructor Destroy; override;
  end;

function IsClockwise(const pts: TArrayOfPoint): boolean;
function Area(const pts: TArrayOfPoint): double;

implementation

const
  horizontal: double = -3.4e+38;

resourcestring
  rsMissingRightbound = 'InsertLocalMinimaIntoAEL: missing rightbound';
  rsDoMaxima = 'DoMaxima error';
  rsUpdateEdgeIntoAEL = 'UpdateEdgeIntoAEL error';
  rsHorizontal = 'ProcessHorizontal error';

//------------------------------------------------------------------------------
// Miscellaneous Functions ...
//------------------------------------------------------------------------------

function IsClockwise(const pts: TArrayOfPoint): boolean; overload;
var
  i, highI: integer;
  area: double;
begin
  result := true;
  highI := high(pts);
  if highI < 2 then exit;
  //or ...(x2-x1)(y2+y1)
  area := int64(pts[highI].x) * pts[0].y - int64(pts[0].x) * pts[highI].y;
  for i := 0 to highI-1 do
    area := area + int64(pts[i].x) * pts[i+1].y - int64(pts[i+1].x) * pts[i].y;
  //area := area/2;
  result := area > 0; //ie reverse of normal formula because Y axis inverted
end;
//------------------------------------------------------------------------------

function Area(const pts: TArrayOfPoint): double;
var
  i, highI: integer;
begin
  result := 0;
  highI := high(pts);
  if highI < 2 then exit;
  result := int64(pts[highI].x) * pts[0].y - int64(pts[0].x) * pts[highI].y;
  for i := 0 to highI-1 do
    result := result + int64(pts[i].x) * pts[i+1].y - int64(pts[i+1].x) * pts[i].y;
end;
//------------------------------------------------------------------------------

function SlopesEqual(e1, e2: PEdge): boolean; overload;
begin
  if (e1.ybot = e1.ytop) then result := (e2.ybot = e2.ytop)
  else if (e2.ybot = e2.ytop) then result := false
  else result :=
    (Int64(e1.ytop-e1.ybot)*(e2.xtop-e2.xbot)-Int64(e1.xtop-e1.xbot)*(e2.ytop-e2.ybot)) = 0;
end;
//---------------------------------------------------------------------------

function SlopesEqual(const pt1, pt2, pt3: TPoint): boolean; overload;
begin
  if (pt1.Y = pt2.Y) then result := (pt2.Y = pt3.Y)
  else if (pt2.Y = pt3.Y) then result := false
  else result := (Int64(pt1.Y-pt2.Y)*(pt2.X-pt3.X)-Int64(pt1.X-pt2.X)*(pt2.Y-pt3.Y)) = 0;
end;
//---------------------------------------------------------------------------

procedure SetDx(e: PEdge);
begin
  if (e.ybot = e.ytop) then e.dx := horizontal
  else e.dx := (e.xtop - e.xbot)/(e.ytop - e.ybot);
end;
//---------------------------------------------------------------------------

function GetDx(const pt1, pt2: TPoint): double;
begin
  if (pt1.Y = pt2.Y) then result := horizontal
  else result := (pt2.X - pt1.X)/(pt2.Y - pt1.Y);
end;
//---------------------------------------------------------------------------

procedure SwapSides(edge1, edge2: PEdge);
var
  side: TEdgeSide;
begin
  side :=  edge1.side;
  edge1.side := edge2.side;
  edge2.side := side;
end;
//------------------------------------------------------------------------------

procedure SwapPolyIndexes(edge1, edge2: PEdge);
var
  outIdx: integer;
begin
  outIdx :=  edge1.outIdx;
  edge1.outIdx := edge2.outIdx;
  edge2.outIdx := outIdx;
end;
//------------------------------------------------------------------------------

function TopX(edge: PEdge; const currentY: integer): integer; overload;
begin
  if currentY = edge.ytop then result := edge.xtop
  else if edge.xtop = edge.xbot then result := edge.xbot
  else result := edge.xbot + round(edge.dx*(currentY - edge.ybot));
end;
//------------------------------------------------------------------------------

function TopX(const pt1, pt2: TPoint; const currentY: integer): integer; overload;
begin
  //preconditions: pt1.Y <> pt2.Y and pt1.Y > pt2.Y
  if currentY >= pt1.Y then result := pt1.X
  else if currentY = pt2.Y then result := pt2.X
  else if pt1.X = pt2.X then result := pt1.X
  else result := pt1.X + trunc(Int64(currentY - pt1.Y)*(pt1.X-pt2.X)/(pt1.Y-pt2.Y));
end;
//------------------------------------------------------------------------------

function IntersectPoint(edge1, edge2: PEdge; out ip: TPoint): boolean;
var
  b1,b2: double;
begin
  if SlopesEqual(edge1, edge2) then
  begin
    result := false;
    exit;
  end;
  if edge1.dx = 0 then
  begin
    ip.X := edge1.xbot;
    if edge2.dx = horizontal then
      ip.Y := edge2.ybot
    else
    begin
      with edge2^ do b2 := ybot - (xbot/dx);
      ip.Y := round(ip.X/edge2.dx + b2);
    end;
  end
  else if edge2.dx = 0 then
  begin
    ip.X := edge2.xbot;
    if edge1.dx = horizontal then
      ip.Y := edge1.ybot
    else
    begin
      with edge1^ do b1 := ybot - (xbot/dx);
      ip.Y := round(ip.X/edge1.dx + b1);
    end;
  end else
  begin
    with edge1^ do b1 := xbot - ybot *dx;
    with edge2^ do b2 := xbot - ybot *dx;
    b2 := (b2-b1)/(edge1.dx - edge2.dx);
    ip.Y := round(b2);
    ip.X := round(edge1.dx * b2 + b1);
  end;
  result :=
    //can be *so close* to the top of one edge that the rounded Y equals one ytop ...
    ((ip.Y = edge1.ytop) and (ip.Y >= edge2.ytop) and (edge1.tmpX > edge2.tmpX)) or
    ((ip.Y = edge2.ytop) and (ip.Y >= edge1.ytop) and (edge1.tmpX > edge2.tmpX)) or
    ((ip.Y > edge1.ytop) and (ip.Y > edge2.ytop));
end;
//------------------------------------------------------------------------------

procedure ReversePolyPtLinks(pp: PPolyPt);
var
  pp1,pp2: PPolyPt;
begin
  pp1 := pp;
  repeat
    pp2:= pp1.next;
    pp1.next := pp1.prev;
    pp1.prev := pp2;
    pp1 := pp2;
  until pp1 = pp;
end;
//------------------------------------------------------------------------------

procedure DisposePolyPts(pp: PPolyPt);
var
  tmpPp: PPolyPt;
begin
  pp.prev.next := nil;
  while assigned(pp) do
  begin
    tmpPp := pp;
    pp := pp.next;
    dispose(tmpPp);
  end;
end;
//------------------------------------------------------------------------------

function IsClockwise(pt: PPolyPt): boolean; overload;
var
  area: double;
  startPt: PPolyPt;
begin
  area := 0;
  startPt := pt;
  repeat
    area := area + int64(pt.pt.X)*pt.next.pt.Y - int64(pt.next.pt.X)*pt.pt.Y;
    pt := pt.next;
  until pt = startPt;
  //area := area /2;
  result := area > 0; //ie reverse of normal formula because Y axis inverted
end;

//------------------------------------------------------------------------------
// TClipperBase4 methods ...
//------------------------------------------------------------------------------

constructor TClipperBase4.Create;
begin
  fEdgeList := TList.Create;
  fLmList := nil;
  fCurrLm := nil;
end;
//------------------------------------------------------------------------------

destructor TClipperBase4.Destroy;
begin
  Clear;
  fEdgeList.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TClipperBase4.AddPolygon(const polygon: TArrayOfPoint; polyType: TPolyType);

  //----------------------------------------------------------------------

  procedure InitEdge(e, eNext, ePrev: PEdge; const pt: TPoint);
  begin
    fillChar(e^, sizeof(TEdge), 0);
    e.next := eNext;
    e.prev := ePrev;
    e.xcurr := pt.X;
    e.ycurr := pt.Y;
    if e.ycurr >= e.next.ycurr then
    begin
      e.xbot := e.xcurr;
      e.ybot := e.ycurr;
      e.xtop := e.next.xcurr;
      e.ytop := e.next.ycurr;
      e.windDelta := 1;
    end else
    begin
      e.xtop := e.xcurr;
      e.ytop := e.ycurr;
      e.xbot := e.next.xcurr;
      e.ybot := e.next.ycurr;
      e.windDelta := -1;
    end;
    SetDx(e);
    e.polyType := polyType;
    e.outIdx := -1;
  end;
  //----------------------------------------------------------------------

  procedure SwapX(e: PEdge);
  begin
    //swap horizontal edges' top and bottom x's so they follow the natural
    //progression of the bounds - ie so their xbots will align with the
    //adjoining lower edge. [Helpful in the ProcessHorizontal() method.]
    e.xcurr := e.xtop;
    e.xtop := e.xbot;
    e.xbot := e.xcurr;
  end;
  //----------------------------------------------------------------------

  procedure InsertLocalMinima(lm: PLocalMinima);
  var
    tmpLm: PLocalMinima;
  begin
    if not assigned(fLmList) then
    begin
      fLmList := lm;
    end
    else if (lm.y >= fLmList.y) then
    begin
      lm.next := fLmList;
      fLmList := lm;
    end else
    begin
      tmpLm := fLmList;
      while assigned(tmpLm.next) and (lm.y < tmpLm.next.y) do
        tmpLm := tmpLm.next;
      lm.next := tmpLm.next;
      tmpLm.next := lm;
    end;
  end;
  //----------------------------------------------------------------------

  function AddBoundsToLML(e: PEdge): PEdge;
  var
    newLm: PLocalMinima;
  begin
    //Starting at the top of one bound we progress to the bottom where there's
    //a local minima. We then go to the top of the next bound. These two bounds
    //form the left and right (or right and left) bounds of the local minima.
    e.nextInLML := nil;
    e := e.next;
    repeat
      if e.dx = horizontal then
      begin
        //nb: proceed through horizontals when approaching from their right,
        //    but break on horizontal minima if approaching from their left.
        //    This ensures 'local minima' are always on the left of horizontals.
        if (e.next.ytop < e.ytop) and (e.next.xbot > e.prev.xbot) then break;
        if (e.xtop <> e.prev.xbot) then SwapX(e);
        e.nextInLML := e.prev;
      end
      else if (e.ybot = e.prev.ybot) then break
      else e.nextInLML := e.prev;
      e := e.next;
    until false;

    //e and e.prev are now at a local minima ...
    new(newLm);
    newLm.y := e.prev.ybot;
    newLm.next := nil;
    if e.dx = horizontal then //horizontal edges never start a left bound
    begin
      if (e.xbot <> e.prev.xbot) then SwapX(e);
      newLm.leftBound := e.prev;
      newLm.rightBound := e;
    end else if (e.dx < e.prev.dx) then
    begin
      newLm.leftBound := e.prev;
      newLm.rightBound := e;
    end else
    begin
      newLm.leftBound := e;
      newLm.rightBound := e.prev;
    end;
    newLm.leftBound.side := esLeft;
    newLm.rightBound.side := esRight;

    InsertLocalMinima(newLm);

    repeat
      if (e.next.ytop = e.ytop) and not (e.next.dx = horizontal) then break;
      e.nextInLML := e.next;
      e := e.next;
      if (e.dx = horizontal) and (e.xbot <> e.prev.xtop) then SwapX(e);
    until false;
    result := e.next;
  end;
  //----------------------------------------------------------------------

var
  i, j, len: integer;
  edges: PEdgeArray;
  e, eHighest: PEdge;
  pg: TArrayOfPoint;
begin
  {AddPolygon}
  len := length(polygon);
  if len < 3 then exit;
  setlength(pg, len);
  pg[0] := polygon[0];
  j := 0;
  for i := 1 to len-1 do
  begin
    if PointsEqual(pg[j], polygon[i]) then continue
    else if (j > 0) and SlopesEqual(pg[j-1], pg[j], polygon[i]) then
    begin
      if PointsEqual(pg[j-1], polygon[i]) then dec(j);
    end else inc(j);
    pg[j] := polygon[i];
  end;
  if (j < 2) then exit;

  len := j+1;
  while true do
  begin
    //nb: test for point equality before testing slopes ...
    if PointsEqual(pg[j], pg[0]) then dec(j)
    else if PointsEqual(pg[0], pg[1]) or SlopesEqual(pg[j], pg[0], pg[1]) then
    begin
      pg[0] := pg[j];
      dec(j);
    end
    else if SlopesEqual(pg[j-1], pg[j], pg[0]) then dec(j)
    else if SlopesEqual(pg[0], pg[1], pg[2]) then
    begin
      for i := 2 to j do pg[i-1] := pg[i];
      dec(j);
    end;
    //exit loop if nothing is changed or there are too few vertices ...
    if (j = len -1) or (j < 2) then break;
    len := j +1;
  end;
  if len < 3 then exit;

  GetMem(edges, sizeof(TEdge)*len);
  fEdgeList.Add(edges);

  //convert vertices to a double-linked-list of edges and initialize ...
  edges[0].xcurr := pg[0].X;
  edges[0].ycurr := pg[0].Y;
  InitEdge(@edges[len-1], @edges[0], @edges[len-2], pg[len-1]);
  for i := len-2 downto 1 do
    InitEdge(@edges[i], @edges[i+1], @edges[i-1], pg[i]);
  InitEdge(@edges[0], @edges[1], @edges[len-1], pg[0]);

  //reset xcurr & ycurr and find 'eHighest' (given the Y axis coordinates
  //increase downward so the 'highest' edge will have the smallest ytop) ...
  e := @edges[0];
  eHighest := e;
  repeat
    e.xcurr := e.xbot;
    e.ycurr := e.ybot;
    if e.ytop < eHighest.ytop then eHighest := e;
    e := e.next;
  until e = @edges[0];

  //make sure eHighest is positioned so the following loop works safely ...
  if eHighest.windDelta > 0 then eHighest := eHighest.next;
  if (eHighest.dx = horizontal) then eHighest := eHighest.next;

  //finally insert each local minima ...
  e := eHighest;
  repeat
    e := AddBoundsToLML(e);
  until (e = eHighest);
end;
//------------------------------------------------------------------------------

procedure TClipperBase4.AddPolygons(const polygons: TArrayOfArrayOfPoint;
  polyType: TPolyType);
var
  i: integer;
begin
  for i := 0 to high(polygons) do AddPolygon(polygons[i], polyType);
end;
//------------------------------------------------------------------------------

procedure TClipperBase4.Clear;
var
  i: Integer;
begin
  DisposeLocalMinimaList;
  for i := 0 to fEdgeList.Count -1 do dispose(PEdgeArray(fEdgeList[i]));
  fEdgeList.Clear;
end;
//------------------------------------------------------------------------------

function TClipperBase4.Reset: boolean;
var
  e: PEdge;
  lm: PLocalMinima;
begin
  //Reset() allows various clipping operations to be executed
  //multiple times on the same polygon sets.

  fCurrLm := fLmList;
  result := assigned(fCurrLm);
  if not result then exit; //ie nothing to process

  //reset all edges ...
  lm := fCurrLm;
  while assigned(lm) do
  begin
    e := lm.leftBound;
    while assigned(e) do
    begin
      e.xcurr := e.xbot;
      e.ycurr := e.ybot;
      e.side := esLeft;
      e.outIdx := -1;
      e := e.nextInLML;
    end;
    e := lm.rightBound;
    while assigned(e) do
    begin
      e.xcurr := e.xbot;
      e.ycurr := e.ybot;
      e.side := esRight;
      e.outIdx := -1;
      e := e.nextInLML;
    end;
    lm := lm.next;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperBase4.DisposeLocalMinimaList;
begin
  while assigned(fLmList) do
  begin
    fCurrLm := fLmList.next;
    Dispose(fLmList);
    fLmList := fCurrLm;
  end;
  fCurrLm := nil;
end;
//------------------------------------------------------------------------------

procedure TClipperBase4.PopLocalMinima;
begin
  if not assigned(fCurrLM) then exit;
  fCurrLM := fCurrLM.next;
end;

//------------------------------------------------------------------------------
// TClipper4 methods ...
//------------------------------------------------------------------------------

constructor TClipper4.Create;
begin
  inherited Create;
  fPolyPtList := TList.Create;
  fHoleInfoList := TList.Create;
end;
//------------------------------------------------------------------------------

destructor TClipper4.Destroy;
begin
  DisposeScanbeamList;
  fPolyPtList.Free;
  fHoleInfoList.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TClipper4.DisposeScanbeamList;
var
  sb: PScanbeam;
begin
  while assigned(fScanbeam) do
  begin
    sb := fScanbeam.next;
    Dispose(fScanbeam);
    fScanbeam := sb;
  end;
end;
//------------------------------------------------------------------------------

function TClipper4.Reset: boolean;
var
  lm: PLocalMinima;
begin
  result := inherited Reset;
  if not result then exit;

  fScanbeam := nil;
  lm := fLmList;
  while assigned(lm) do
  begin
    InsertScanbeam(lm.y);
    InsertScanbeam(lm.leftbound.ytop);
    lm := lm.next;
  end;
end;
//------------------------------------------------------------------------------

function TClipper4.Execute(clipType: TClipType;
  out solution: TArrayOfArrayOfPoint;
  subjFillType: TPolyFillType = pftEvenOdd;
  clipFillType: TPolyFillType = pftEvenOdd): boolean;
var
  botY, topY: integer;
begin
  result := false;
  solution := nil;
  if fExecuteLocked then exit;
  try try
    fExecuteLocked := true;
    fSubjFillType := subjFillType;
    fClipFillType := clipFillType;
    fClipType := clipType;

    if not Reset then exit;
    botY := PopScanbeam;
    repeat
      InsertLocalMinimaIntoAEL(botY);
      ProcessHorizontals;
      topY := PopScanbeam;
      if not ProcessIntersections(topY) then exit;
      ProcessEdgesAtTopOfScanbeam(topY);
      botY := topY;
    until fScanbeam = nil;
    solution := GetResult;
    result := true;
  except
    //result := false;
  end;
  finally
    DisposeAllPolyPts;
    fExecuteLocked := false;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper4.InsertScanbeam(const y: integer);
var
  sb, sb2: PScanbeam;
begin
  new(sb);
  sb.y := y;
  if not assigned(fScanbeam) then
  begin
    fScanbeam := sb;
    sb.next := nil;
  end else if y > fScanbeam.y then
  begin
    sb.next := fScanbeam;
    fScanbeam := sb;
  end else
  begin
    sb2 := fScanbeam;
    while assigned(sb2.next) and (y <= sb2.next.y) do sb2 := sb2.next;
    if y <> sb2.y then
    begin
      sb.next := sb2.next;
      sb2.next := sb;
    end
    else dispose(sb); //ie ignores duplicates
  end;
end;
//------------------------------------------------------------------------------

function TClipper4.PopScanbeam: integer;
var
  sb: PScanbeam;
begin
  result := fScanbeam.y;
  sb := fScanbeam;
  fScanbeam := fScanbeam.next;
  dispose(sb);
end;
//------------------------------------------------------------------------------

procedure TClipper4.DisposeAllPolyPts;
var
  i: integer;
begin
  for i := 0 to fPolyPtList.Count -1 do
  begin
    Dispose(PHoleInfo(fHoleInfoList[i]));
    if assigned(fPolyPtList[i]) then
      DisposePolyPts(PPolyPt(fPolyPtList[i]));
  end;
  fPolyPtList.Clear;
  fHoleInfoList.Clear;
end;
//------------------------------------------------------------------------------

procedure TClipper4.SetWindingCount(edge: PEdge);
var
  e: PEdge;
begin
  e := edge.prevInAEL;
  //find the edge of the same polytype that immediately preceeds 'edge' in AEL
  while assigned(e) and (e.polyType <> edge.polyType) do e := e.prevInAEL;
  if not assigned(e) then
  begin
    edge.windCnt := edge.windDelta;
    edge.windCnt2 := 0;
    e := fActiveEdges; //ie get ready to calc windCnt2
  end else if IsNonZeroFillType(edge) then
  begin
    //nonZero filling ...
    if e.windCnt * e.windDelta < 0 then
    begin
      if (abs(e.windCnt) > 1) then
      begin
        if (e.windDelta * edge.windDelta < 0) then edge.windCnt := e.windCnt
        else edge.windCnt := e.windCnt + edge.windDelta;
      end else
        edge.windCnt := e.windCnt + e.windDelta + edge.windDelta;
    end else
    begin
      if (abs(e.windCnt) > 1) and (e.windDelta * edge.windDelta < 0) then
        edge.windCnt := e.windCnt
      else if e.windCnt + edge.windDelta = 0 then
        edge.windCnt := e.windCnt
      else edge.windCnt := e.windCnt + edge.windDelta;
    end;
    edge.windCnt2 := e.windCnt2;
    e := e.nextInAEL; //ie get ready to calc windCnt2
  end else
  begin
    //even-odd filling ...
    edge.windCnt := 1;
    edge.windCnt2 := e.windCnt2;
    e := e.nextInAEL; //ie get ready to calc windCnt2
  end;

  //update windCnt2 ...
  if IsNonZeroAltFillType(edge) then
  begin
    //nonZero filling ...
    while (e <> edge) do
    begin
      inc(edge.windCnt2, e.windDelta);
      e := e.nextInAEL;
    end;
  end else
  begin
    //even-odd filling ...
    while (e <> edge) do
    begin
      if edge.windCnt2 = 0 then edge.windCnt2 := 1 else edge.windCnt2 := 0;
      e := e.nextInAEL;
    end;
  end;
end;
//------------------------------------------------------------------------------

function TClipper4.IsNonZeroFillType(edge: PEdge): boolean;
begin
  if edge.polyType = ptSubject then
    result := fSubjFillType = pftNonZero else
    result := fClipFillType = pftNonZero;
end;
//------------------------------------------------------------------------------

function TClipper4.IsNonZeroAltFillType(edge: PEdge): boolean;
begin
  if edge.polyType = ptSubject then
    result := fClipFillType = pftNonZero else
    result := fSubjFillType = pftNonZero;
end;
//------------------------------------------------------------------------------

function TClipper4.IsContributing(edge: PEdge): boolean;
begin
  result := true;
  case fClipType of
    ctIntersection:
      begin
        if edge.polyType = ptSubject then
          result := (abs(edge.windCnt) = 1) and (edge.windCnt2 <> 0) else
          result := (edge.windCnt2 <> 0) and (abs(edge.windCnt) = 1);
      end;
    ctUnion:
      begin
        result := (abs(edge.windCnt) = 1) and (edge.windCnt2 = 0);
      end;
    ctDifference:
      begin
        if edge.polyType = ptSubject then
        result := (abs(edge.windCnt) = 1) and (edge.windCnt2 = 0) else
        result := (abs(edge.windCnt) = 1) and (edge.windCnt2 <> 0);
      end;
    ctXor:
      result := (abs(edge.windCnt) = 1);
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper4.AddLocalMinPoly(e1, e2: PEdge; const pt: TPoint);
begin
  AddPolyPt(e1, pt);
  e2.outIdx := e1.outIdx;
  if (e2.dx = horizontal)  or (e1.dx > e2.dx) then
  begin
    e1.side := esLeft;
    e2.side := esRight;
  end else
  begin
    e1.side := esRight;
    e2.side := esLeft;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper4.AddLocalMaxPoly(e1, e2: PEdge; const pt: TPoint);
begin
  AddPolyPt(e1, pt);
  if (e1.outIdx = e2.outIdx) then
  begin
    e1.outIdx := -1;
    e2.outIdx := -1;
  end else
    AppendPolygon(e1, e2);
end;
//------------------------------------------------------------------------------

procedure TClipper4.AddEdgeToSEL(edge: PEdge);
begin
  //SEL pointers in PEdge are reused to build a list of horizontal edges.
  //However, we don't need to worry about order with horizontal edge processing.
  if not assigned(fSortedEdges) then
  begin
    fSortedEdges := edge;
    edge.prevInSEL := nil;
    edge.nextInSEL := nil;
  end else
  begin
    edge.nextInSEL := fSortedEdges;
    edge.prevInSEL := nil;
    fSortedEdges.prevInSEL := edge;
    fSortedEdges := edge;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper4.CopyAELToSEL;
var
  e: PEdge;
begin
  e := fActiveEdges;
  fSortedEdges := e;
  if not assigned(fActiveEdges) then exit;

  fSortedEdges.prevInSEL := nil;
  e := e.nextInAEL;
  while assigned(e) do
  begin
    e.prevInSEL := e.prevInAEL;
    e.prevInSEL.nextInSEL := e;
    e.nextInSEL := nil;
    e := e.nextInAEL;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper4.InsertLocalMinimaIntoAEL(const botY: integer);

  function E2InsertsBeforeE1(e1,e2: PEdge): boolean;
  begin
    if e2.xcurr = e1.xcurr then result := e2.dx > e1.dx
    else result := e2.xcurr < e1.xcurr;
  end;
  //----------------------------------------------------------------------

  procedure InsertEdgeIntoAEL(edge: PEdge);
  var
    e: PEdge;
  begin
    edge.prevInAEL := nil;
    edge.nextInAEL := nil;
    if not assigned(fActiveEdges) then
    begin
      fActiveEdges := edge;
    end else if E2InsertsBeforeE1(fActiveEdges, edge) then
    begin
      edge.nextInAEL := fActiveEdges;
      fActiveEdges.prevInAEL := edge;
      fActiveEdges := edge;
    end else
    begin
      e := fActiveEdges;
      while assigned(e.nextInAEL) and not E2InsertsBeforeE1(e.nextInAEL, edge) do
        e := e.nextInAEL;
      edge.nextInAEL := e.nextInAEL;
      if assigned(e.nextInAEL) then e.nextInAEL.prevInAEL := edge;
      edge.prevInAEL := e;
      e.nextInAEL := edge;
    end;
  end;
  //----------------------------------------------------------------------

var
  e: PEdge;
  pt: TPoint;
  lb, rb: PEdge;
begin
  while assigned(CurrentLm) and (CurrentLm.Y = botY) do
  begin
    lb := CurrentLm.leftBound;
    rb := CurrentLm.rightBound;

    InsertEdgeIntoAEL(lb);
    InsertScanbeam(lb.ytop);
    InsertEdgeIntoAEL(rb);

    //set edge winding states ...
    if IsNonZeroFillType(lb) then
    begin
      rb.windDelta := -lb.windDelta
    end else
    begin
      lb.windDelta := 1;
      rb.windDelta := 1;
    end;
    SetWindingCount(lb);
    rb.windCnt := lb.windCnt;
    rb.windCnt2 := lb.windCnt2;

    if rb.dx = horizontal then
    begin
      AddEdgeToSEL(rb);
      InsertScanbeam(rb.nextInLML.ytop);
    end else
      InsertScanbeam(rb.ytop);

    if IsContributing(lb) then
      AddLocalMinPoly(lb, rb, Point(lb.xcurr, CurrentLm.y));

    if (lb.nextInAEL <> rb) then
    begin
      e := lb.nextInAEL;
      pt := Point(lb.xcurr,lb.ycurr);
      while e <> rb do
      begin
        if not assigned(e) then raise exception.Create(rsMissingRightbound);
        //nb: For calculating winding counts etc, IntersectEdges() assumes
        //that param1 will be to the right of param2 ABOVE the intersection ...
        IntersectEdges(rb, e, pt);
        e := e.nextInAEL;
      end;
    end;
    PopLocalMinima;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper4.DeleteFromAEL(e: PEdge);
var
  AelPrev, AelNext: PEdge;
begin
  AelPrev := e.prevInAEL;
  AelNext := e.nextInAEL;
  if not assigned(AelPrev) and not assigned(AelNext) and
    (e <> fActiveEdges) then exit; //already deleted
  if assigned(AelPrev) then AelPrev.nextInAEL := AelNext
  else fActiveEdges := AelNext;
  if assigned(AelNext) then AelNext.prevInAEL := AelPrev;
  e.nextInAEL := nil;
  e.prevInAEL := nil;
end;
//------------------------------------------------------------------------------

procedure TClipper4.DeleteFromSEL(e: PEdge);
var
  SelPrev, SelNext: PEdge;
begin
  SelPrev := e.prevInSEL;
  SelNext := e.nextInSEL;
  if not assigned(SelPrev) and not assigned(SelNext) and
    (e <> fSortedEdges) then exit; //already deleted
  if assigned(SelPrev) then SelPrev.nextInSEL := SelNext
  else fSortedEdges := SelNext;
  if assigned(SelNext) then SelNext.prevInSEL := SelPrev;
  e.nextInSEL := nil;
  e.prevInSEL := nil;
end;
//------------------------------------------------------------------------------

procedure TClipper4.IntersectEdges(e1,e2: PEdge;
  const pt: TPoint; protects: TIntersectProtects = []);

  procedure DoEdge1;
  begin
    AddPolyPt(e1, pt);
    SwapSides(e1, e2);
    SwapPolyIndexes(e1, e2);
  end;
  //----------------------------------------------------------------------

  procedure DoEdge2;
  begin
    AddPolyPt(e2, pt);
    SwapSides(e1, e2);
    SwapPolyIndexes(e1, e2);
  end;
  //----------------------------------------------------------------------

  procedure DoBothEdges;
  begin
    AddPolyPt(e1, pt);
    AddPolyPt(e2, pt);
    SwapSides(e1, e2);
    SwapPolyIndexes(e1, e2);
  end;
  //----------------------------------------------------------------------

var
  oldE1WindCnt: integer;
  e1stops, e2stops: boolean;
  e1Contributing, e2contributing: boolean;
begin
  {IntersectEdges}

  //e1 will be to the left of e2 BELOW the intersection. Therefore e1 is before
  //e2 in AEL except when e1 is being inserted at the intersection point ...

  e1stops := not (ipLeft in protects) and not assigned(e1.nextInLML) and
    (e1.xtop = pt.x) and (e1.ytop = pt.y);
  e2stops := not (ipRight in protects) and not assigned(e2.nextInLML) and
    (e2.xtop = pt.x) and (e2.ytop = pt.y);
  e1Contributing := (e1.outIdx >= 0);
  e2contributing := (e2.outIdx >= 0);

  //update winding counts...
  //assumes that e1 will be to the right of e2 ABOVE the intersection
  if e1.polyType = e2.polyType then
  begin
    if IsNonZeroFillType(e1) then
    begin
      if e1.windCnt + e2.windDelta = 0 then
        e1.windCnt := -e1.windCnt else
        inc(e1.windCnt, e2.windDelta);
      if e2.windCnt - e1.windDelta = 0 then
        e2.windCnt := -e2.windCnt else
        dec(e2.windCnt, e1.windDelta);
    end else
    begin
      oldE1WindCnt := e1.windCnt;
      e1.windCnt := e2.windCnt;
      e2.windCnt := oldE1WindCnt;
    end;
  end else
  begin
    if IsNonZeroFillType(e2) then inc(e1.windCnt2, e2.windDelta)
    else if e1.windCnt2 = 0 then e1.windCnt2 := 1
    else e1.windCnt2 := 0;
    if IsNonZeroFillType(e1) then dec(e2.windCnt2, e1.windDelta)
    else if e2.windCnt2 = 0 then e2.windCnt2 := 1
    else e2.windCnt2 := 0;
  end;

  if e1Contributing and e2contributing then
  begin
    if e1stops or e2stops or
      (abs(e1.windCnt) > 1) or (abs(e2.windCnt) > 1) or
      ((e1.polytype <> e2.polytype) and (fClipType <> ctXor)) then
        AddLocalMaxPoly(e1, e2, pt) else
        DoBothEdges;
  end
  else if e1Contributing then
  begin
    case fClipType of
      ctIntersection: if (abs(e2.windCnt) < 2) and
        ((e2.polyType = ptSubject) or (e2.windCnt2 <> 0)) then DoEdge1;
      else
        if (abs(e2.windCnt) < 2) then DoEdge1;
    end;
  end
  else if e2contributing then
  begin
    case fClipType of
      ctIntersection: if (abs(e1.windCnt) < 2) and
        ((e1.polyType = ptSubject) or (e1.windCnt2 <> 0)) then DoEdge2;
      else
        if (abs(e1.windCnt) < 2) then DoEdge2;
    end;
  end
  else
  begin
    //neither edge is currently contributing ...
    if (abs(e1.windCnt) > 1) and (abs(e2.windCnt) > 1) then
      // do nothing
    else if (e1.polytype <> e2.polytype) and
      not e1stops and not e2stops and
      (abs(e1.windCnt) < 2) and (abs(e2.windCnt) < 2)then
      AddLocalMinPoly(e1, e2, pt)
    else if (abs(e1.windCnt) = 1) and (abs(e2.windCnt) = 1) then
      case fClipType of
        ctIntersection:
          if (abs(e1.windCnt2) > 0) and (abs(e2.windCnt2) > 0) then
            AddLocalMinPoly(e1, e2, pt);
        ctUnion:
          if (e1.windCnt2 = 0) and (e2.windCnt2 = 0) then
            AddLocalMinPoly(e1, e2, pt);
        ctDifference:
          if ((e1.polyType = ptClip) and (e2.polyType = ptClip) and
            (e1.windCnt2 <> 0) and (e2.windCnt2 <> 0)) or
            ((e1.polyType = ptSubject) and (e2.polyType = ptSubject) and
            (e1.windCnt2 = 0) and (e2.windCnt2 = 0)) then
              AddLocalMinPoly(e1, e2, pt);
        ctXor:
          AddLocalMinPoly(e1, e2, pt);
      end
    else if (abs(e1.windCnt) < 2) and (abs(e2.windCnt) < 2) then
      swapsides(e1,e2);
  end;

  if (e1stops <> e2stops) and
    ((e1stops and (e1.outIdx >= 0)) or (e2stops and (e2.outIdx >= 0))) then
  begin
    swapsides(e1,e2);
    SwapPolyIndexes(e1, e2);
  end;

  //finally, delete any non-contributing maxima edges  ...
  if e1stops then deleteFromAEL(e1);
  if e2stops then deleteFromAEL(e2);
end;
//------------------------------------------------------------------------------

procedure TClipper4.AppendPolygon(e1, e2: PEdge);
var
  p1_lft, p1_rt, p2_lft, p2_rt: PPolyPt;
  newSide: TEdgeSide;
  OKIdx, ObsoleteIdx: integer;
  e: PEdge;
begin
  PHoleInfo(fHoleInfoList[e2.outIdx]).idx := e1.outIdx;

  //get the start and ends of both output polygons ...
  p1_lft := PPolyPt(fPolyPtList[e1.outIdx]);
  p1_rt := p1_lft.prev;
  p2_lft := PPolyPt(fPolyPtList[e2.outIdx]);
  p2_rt := p2_lft.prev;

  //join e2 poly onto e1 poly and delete pointers to e2 ...
  if e1.side = esLeft then
  begin
    if e2.side = esLeft then
    begin
      //z y x a b c
      ReversePolyPtLinks(p2_lft);
      p2_lft.next := p1_lft;
      p1_lft.prev := p2_lft;
      p1_rt.next := p2_rt;
      p2_rt.prev := p1_rt;
      fPolyPtList[e1.outIdx] := p2_rt;
    end else
    begin
      //x y z a b c
      p2_rt.next := p1_lft;
      p1_lft.prev := p2_rt;
      p2_lft.prev := p1_rt;
      p1_rt.next := p2_lft;
      fPolyPtList[e1.outIdx] := p2_lft;
    end;
    newSide := esLeft;
  end else
  begin
    if e2.side = esRight then
    begin
      //a b c z y x
      ReversePolyPtLinks(p2_lft);
      p1_rt.next := p2_rt;
      p2_rt.prev := p1_rt;
      p2_lft.next := p1_lft;
      p1_lft.prev := p2_lft;
    end else
    begin
      //a b c x y z
      p1_rt.next := p2_lft;
      p2_lft.prev := p1_rt;
      p1_lft.prev := p2_rt;
      p2_rt.next := p1_lft;
    end;
    newSide := esRight;
  end;

  OKIdx := e1.outIdx;
  ObsoleteIdx := e2.outIdx;
  fPolyPtList[ObsoleteIdx] := nil;

  e1.outIdx := -1; //nb: safe because we only get here via AddLocalMaxPoly
  e2.outIdx := -1;

  e := fActiveEdges;
  while assigned(e) do
  begin
    if (e.outIdx = ObsoleteIdx) then
    begin
      e.outIdx := OKIdx;
      e.side := newSide;
      break;
    end;
    e := e.nextInAEL;
  end;
end;
//------------------------------------------------------------------------------

function TClipper4.AddPolyPt(e: PEdge; const pt: TPoint): PPolyPt;
var
  fp: PPolyPt;
  ToFront: boolean;
  hi: PHoleInfo;
begin
  ToFront := e.side = esLeft;
  if e.outIdx < 0 then
  begin
    new(result);
    e.outIdx := fPolyPtList.Add(result);
    result.pt := pt;
    result.next := result;
    result.prev := result;
    result.isDone := false;

    new(hi);
    hi.pp := result;
    hi.isHole := false;
    hi.idx := e.outIdx;
    hi.IsDone := false;
    fHoleInfoList.Add(hi);
  end else
  begin
    result := nil;
    fp := PPolyPt(fPolyPtList[e.outIdx]);
    if (ToFront and PointsEqual(pt, fp.pt)) then result := fp
    else if not ToFront and PointsEqual(pt, fp.prev.pt) then result := fp.prev;
    if assigned(result) then exit;

    new(result);
    result.pt := pt;
    result.next := fp;
    result.prev := fp.prev;
    result.prev.next := result;
    result.isDone := false;
    fp.prev := result;
    if ToFront then fPolyPtList[e.outIdx] := result;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper4.ProcessHorizontals;
var
  e: PEdge;
begin
  while assigned(fSortedEdges) do
  begin
    e := fSortedEdges;
    DeleteFromSEL(e);
    ProcessHorizontal(e);
  end;
end;
//------------------------------------------------------------------------------

function TClipper4.IsTopHorz(const XPos: integer): boolean;
var
  e: PEdge;
begin
  result := false;
  e := fSortedEdges;
  while assigned(e) do
  begin
    if (XPos >= min(e.xcurr,e.xtop)) and (XPos <= max(e.xcurr,e.xtop)) then exit;
    e := e.nextInSEL;
  end;
  result := true;
end;
//------------------------------------------------------------------------------

function IsMinima(e: PEdge): boolean;
begin
  result := assigned(e) and (e.prev.nextInLML <> e) and (e.next.nextInLML <> e);
end;
//------------------------------------------------------------------------------

function IsMaxima(e: PEdge; const Y: integer): boolean;
begin
  result := assigned(e) and (e.ytop = Y) and not assigned(e.nextInLML);
end;
//------------------------------------------------------------------------------

function IsIntermediate(e: PEdge; const Y: integer): boolean;
begin
  result := (e.ytop = Y) and assigned(e.nextInLML);
end;
//------------------------------------------------------------------------------

function GetMaximaPair(e: PEdge): PEdge;
begin
  result := e.next;
  if not IsMaxima(result, e.ytop) or (result.xtop <> e.xtop) then
    result := e.prev;
end;
//------------------------------------------------------------------------------

procedure TClipper4.SwapPositionsInAEL(e1, e2: PEdge);
var
  prev,next: PEdge;
begin
  with e1^ do if not assigned(nextInAEL) and not assigned(prevInAEL) then exit;
  with e2^ do if not assigned(nextInAEL) and not assigned(prevInAEL) then exit;

  if e1.nextInAEL = e2 then
  begin
    next := e2.nextInAEL;
    if assigned(next) then next.prevInAEL := e1;
    prev := e1.prevInAEL;
    if assigned(prev) then prev.nextInAEL := e2;
    e2.prevInAEL := prev;
    e2.nextInAEL := e1;
    e1.prevInAEL := e2;
    e1.nextInAEL := next;
  end
  else if e2.nextInAEL = e1 then
  begin
    next := e1.nextInAEL;
    if assigned(next) then next.prevInAEL := e2;
    prev := e2.prevInAEL;
    if assigned(prev) then prev.nextInAEL := e1;
    e1.prevInAEL := prev;
    e1.nextInAEL := e2;
    e2.prevInAEL := e1;
    e2.nextInAEL := next;
  end else
  begin
    next := e1.nextInAEL;
    prev := e1.prevInAEL;
    e1.nextInAEL := e2.nextInAEL;
    if assigned(e1.nextInAEL) then e1.nextInAEL.prevInAEL := e1;
    e1.prevInAEL := e2.prevInAEL;
    if assigned(e1.prevInAEL) then e1.prevInAEL.nextInAEL := e1;
    e2.nextInAEL := next;
    if assigned(e2.nextInAEL) then e2.nextInAEL.prevInAEL := e2;
    e2.prevInAEL := prev;
    if assigned(e2.prevInAEL) then e2.prevInAEL.nextInAEL := e2;
  end;
  if not assigned(e1.prevInAEL) then fActiveEdges := e1
  else if not assigned(e2.prevInAEL) then fActiveEdges := e2;
end;
//------------------------------------------------------------------------------

procedure TClipper4.SwapPositionsInSEL(e1, e2: PEdge);
var
  prev,next: PEdge;
begin
  if e1.nextInSEL = e2 then
  begin
    next    := e2.nextInSEL;
    if assigned(next) then next.prevInSEL := e1;
    prev    := e1.prevInSEL;
    if assigned(prev) then prev.nextInSEL := e2;
    e2.prevInSEL := prev;
    e2.nextInSEL := e1;
    e1.prevInSEL := e2;
    e1.nextInSEL := next;
  end
  else if e2.nextInSEL = e1 then
  begin
    next    := e1.nextInSEL;
    if assigned(next) then next.prevInSEL := e2;
    prev    := e2.prevInSEL;
    if assigned(prev) then prev.nextInSEL := e1;
    e1.prevInSEL := prev;
    e1.nextInSEL := e2;
    e2.prevInSEL := e1;
    e2.nextInSEL := next;
  end else
  begin
    next    := e1.nextInSEL;
    prev    := e1.prevInSEL;
    e1.nextInSEL := e2.nextInSEL;
    if assigned(e1.nextInSEL) then e1.nextInSEL.prevInSEL := e1;
    e1.prevInSEL := e2.prevInSEL;
    if assigned(e1.prevInSEL) then e1.prevInSEL.nextInSEL := e1;
    e2.nextInSEL := next;
    if assigned(e2.nextInSEL) then e2.nextInSEL.prevInSEL := e2;
    e2.prevInSEL := prev;
    if assigned(e2.prevInSEL) then e2.prevInSEL.nextInSEL := e2;
  end;
  if not assigned(e1.prevInSEL) then fSortedEdges := e1
  else if not assigned(e2.prevInSEL) then fSortedEdges := e2;
end;
//------------------------------------------------------------------------------

procedure TClipper4.ProcessHorizontal(horzEdge: PEdge);

  function GetNextInAEL(e: PEdge; Direction: TDirection): PEdge;
  begin
    if Direction = dLeftToRight then
      result := e.nextInAEL else
      result := e.prevInAEL;
  end;
  //------------------------------------------------------------------------

var
  e, eNext, eMaxPair: PEdge;
  horzLeft, horzRight: integer;
  Direction: TDirection;
const
  ProtectLeft: array[boolean] of TIntersectProtects = ([ipRight], [ipLeft,ipRight]);
  ProtectRight: array[boolean] of TIntersectProtects = ([ipLeft], [ipLeft,ipRight]);
begin
(*******************************************************************************
* Notes: Horizontal edges (HEs) at scanline intersections (ie at the top or    *
* bottom of a scanbeam) are processed as if layered. The order in which HEs    *
* are processed doesn't matter. HEs intersect with other HE xbots only [#],    *
* and with other non-horizontal edges [*]. Once these intersections are        *
* processed, intermediate HEs then 'promote' the edge above (nextInLML) into   *
* the AEL. These 'promoted' edges may in turn intersect [%] with other HEs.    *
*******************************************************************************)

(*******************************************************************************
*           \   nb: HE processing order doesn't matter         /          /    *
*            \                                                /          /     *
* { --------  \  -------------------  /  \  - (3) o==========%==========o  - } *
* {            o==========o (2)      /    \       .          .               } *
* {                       .         /      \      .          .               } *
* { ----  o===============#========*========*=====#==========o  (1)  ------- } *
*        /                 \      /          \   /                             *
*******************************************************************************)

  if horzEdge.xcurr < horzEdge.xtop then
  begin
    horzLeft := horzEdge.xcurr; horzRight := horzEdge.xtop;
    Direction := dLeftToRight;
  end else
  begin
    horzLeft := horzEdge.xtop; horzRight := horzEdge.xcurr;
    Direction := dRightToLeft;
  end;

  if assigned(horzEdge.nextInLML) then
    eMaxPair := nil else
    eMaxPair := GetMaximaPair(horzEdge);

  e := GetNextInAEL(horzEdge, Direction);
  while assigned(e) do
  begin
    eNext := GetNextInAEL(e, Direction);
    if (e.xcurr >= horzLeft) and (e.xcurr <= horzRight) then
    begin
      //ok, so far it looks like we're still in range of the horizontal edge

      if (e.xcurr = horzEdge.xtop) and
        assigned(horzEdge.nextInLML) then
      begin
        if SlopesEqual(e, horzEdge.nextInLML) then
        begin
          break; //we've reached the end of the horizontal line
        end
        else if (e.dx < horzEdge.nextInLML.dx) then
        //we really have got to the end of the intermediate horz edge so quit.
        //nb: More -ve slopes follow more +ve slopes ABOVE the horizontal.
          break;
      end;

      if (e = eMaxPair) then
      begin
        //horzEdge is evidently a maxima horizontal and we've arrived at its end.
        if Direction = dLeftToRight then
          IntersectEdges(horzEdge, e, Point(e.xcurr, horzEdge.ycurr)) else
          IntersectEdges(e, horzEdge, Point(e.xcurr, horzEdge.ycurr));
        exit;
      end
      else if (e.dx = horizontal) and not IsMinima(e) and not (e.xcurr > e.xtop) then
      begin
        //An overlapping horizontal edge. Overlapping horizontal edges are
        //processed as if layered with the current horizontal edge (horizEdge)
        //being infinitesimally lower that the next (e). Therfore, we
        //intersect with e only if e.xcurr is within the bounds of horzEdge ...
        if Direction = dLeftToRight then
          IntersectEdges(horzEdge, e, Point(e.xcurr, horzEdge.ycurr),
            ProtectRight[not IsTopHorz(e.xcurr)])
        else
          IntersectEdges(e, horzEdge, Point(e.xcurr, horzEdge.ycurr),
            ProtectLeft[not IsTopHorz(e.xcurr)]);
      end
      else if (Direction = dLeftToRight) then
      begin
        IntersectEdges(horzEdge, e, Point(e.xcurr, horzEdge.ycurr),
          ProtectRight[not IsTopHorz(e.xcurr)])
      end else
      begin
        IntersectEdges(e, horzEdge, Point(e.xcurr, horzEdge.ycurr),
          ProtectLeft[not IsTopHorz(e.xcurr)]);
      end;
      SwapPositionsInAEL(horzEdge, e);
    end
    else if (Direction = dLeftToRight) and
      (e.xcurr > horzRight) and assigned(fSortedEdges) then break
    else if (Direction = dRightToLeft) and
      (e.xcurr < horzLeft) and assigned(fSortedEdges) then break;
    e := eNext;
  end;

  if assigned(horzEdge.nextInLML) then
  begin
    if (horzEdge.outIdx >= 0) then
      AddPolyPt(horzEdge, Point(horzEdge.xtop, horzEdge.ytop));
    UpdateEdgeIntoAEL(horzEdge);
  end else
  begin
    if horzEdge.outIdx >= 0 then
      IntersectEdges(horzEdge, eMaxPair,
        Point(horzEdge.xtop, horzEdge.ycurr), [ipLeft,ipRight]);
    if eMaxPair.outIdx >= 0 then raise exception.Create(rsHorizontal);
    DeleteFromAEL(eMaxPair);
    DeleteFromAEL(horzEdge);
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper4.UpdateEdgeIntoAEL(var e: PEdge);
var
  AelPrev, AelNext: PEdge;
begin
  if not assigned(e.nextInLML) then raise exception.Create(rsUpdateEdgeIntoAEL);
  AelPrev := e.prevInAEL;
  AelNext := e.nextInAEL;
  e.nextInLML.outIdx := e.outIdx;
  if assigned(AelPrev) then
    AelPrev.nextInAEL := e.nextInLML else
    fActiveEdges := e.nextInLML;
  if assigned(AelNext) then
    AelNext.prevInAEL := e.nextInLML;
  e.nextInLML.side := e.side;
  e.nextInLML.windDelta := e.windDelta;
  e.nextInLML.windCnt := e.windCnt;
  e.nextInLML.windCnt2 := e.windCnt2;
  e := e.nextInLML;
  e.prevInAEL := AelPrev;
  e.nextInAEL := AelNext;
  if e.dx <> horizontal then
    InsertScanbeam(e.ytop);
end;
//------------------------------------------------------------------------------

function TClipper4.ProcessIntersections(const topY: integer): boolean;
begin
  result := true;
  try
    BuildIntersectList(topY);
    if fIntersectNodes = nil then exit;
    if FixupIntersections then ProcessIntersectList
    else result := false;
  finally
    //if there's been an error, clean up the mess ...
    DisposeIntersectNodes;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper4.DisposeIntersectNodes;
var
  n: PIntersectNode;
begin
  while assigned(fIntersectNodes) do
  begin
    n := fIntersectNodes.next;
    dispose(fIntersectNodes);
    fIntersectNodes := n;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper4.BuildIntersectList(const topY: integer);
var
  e, eNext: PEdge;
  pt: TPoint;
  isModified: boolean;
begin
  if not assigned(fActiveEdges) then exit;

  //prepare for sorting ...
  e := fActiveEdges;
  e.tmpX := TopX(e, topY);
  fSortedEdges := e;
  fSortedEdges.prevInSEL := nil;
  e := e.nextInAEL;
  while assigned(e) do
  begin
    e.prevInSEL := e.prevInAEL;
    e.prevInSEL.nextInSEL := e;
    e.nextInSEL := nil;
    e.tmpX := TopX(e, topY);
    e := e.nextInAEL;
  end;

  try
    //bubblesort ...
    isModified := true;
    while isModified and assigned(fSortedEdges) do
    begin
      isModified := false;
      e := fSortedEdges;
      while assigned(e.nextInSEL) do
      begin
        eNext := e.nextInSEL;
        if (e.tmpX > eNext.tmpX) and IntersectPoint(e, eNext, pt) then
        begin
          AddIntersectNode(e, eNext, pt);
          SwapPositionsInSEL(e, eNext);
          isModified := true;
        end else
          e := eNext;
      end;
      if assigned(e.prevInSEL) then e.prevInSEL.nextInSEL := nil else break;
    end;
  finally
    fSortedEdges := nil;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper4.AddIntersectNode(e1, e2: PEdge; const pt: TPoint);

  function Process1Before2(node1, node2: PIntersectNode): boolean;
  begin
    if node1.pt.Y = node2.pt.Y then
    begin
      if (node1.edge1 = node2.edge1) or (node1.edge2 = node2.edge1) then
      begin
        result := node2.pt.X > node1.pt.X;
        if node2.edge1.dx > 0 then result := not result;
      end
      else if (node1.edge1 = node2.edge2) or (node1.edge2 = node2.edge2) then
      begin
        result := node2.pt.X > node1.pt.X;
        if node2.edge2.dx > 0 then result := not result;
      end else
        result := node2.pt.X > node1.pt.X;
    end
    else result := node1.pt.Y > node2.pt.Y;
  end;
  //----------------------------------------------------------------------------

var
  node, newNode: PIntersectNode;
begin
  new(newNode);
  newNode.edge1 := e1;
  newNode.edge2 := e2;
  newNode.pt := pt;
  newNode.next := nil;
  if not assigned(fIntersectNodes) then
    fIntersectNodes := newNode
  else if Process1Before2(newNode, fIntersectNodes) then
  begin
    newNode.next := fIntersectNodes;
    fIntersectNodes := newNode;
  end else
  begin
    node := fIntersectNodes;
    while assigned(node.next) and
      Process1Before2(node.next, newNode) do
      node := node.next;
    newNode.next := node.next;
    node.next := newNode;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper4.ProcessIntersectList;
var
  node: PIntersectNode;
begin
  while assigned(fIntersectNodes) do
  begin
    node := fIntersectNodes.next;
    with fIntersectNodes^ do
    begin
      IntersectEdges(edge1, edge2, pt, [ipLeft,ipRight]);
      SwapPositionsInAEL(edge1, edge2);
    end;
    dispose(fIntersectNodes);
    fIntersectNodes := node;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper4.DoMaxima(e: PEdge; const topY: integer);
var
  eNext, eMaxPair: PEdge;
  X: integer;
begin
  eMaxPair := GetMaximaPair(e);
  X := e.xtop;
  eNext := e.nextInAEL;
  while eNext <> eMaxPair do
  begin
    if not assigned(eNext) then raise exception.Create(rsDoMaxima);
    IntersectEdges(e, eNext, Point(X, topY), [ipLeft, ipRight]);
    eNext := eNext.nextInAEL;
  end;
  if (e.outIdx < 0) and (eMaxPair.outIdx < 0) then
  begin
    DeleteFromAEL(e);
    DeleteFromAEL(eMaxPair);
  end
  else if (e.outIdx >= 0) and (eMaxPair.outIdx >= 0) then
  begin
    IntersectEdges(e, eMaxPair, Point(X, topY));
  end
  else raise exception.Create(rsDoMaxima);
end;
//------------------------------------------------------------------------------

procedure TClipper4.ProcessEdgesAtTopOfScanbeam(const topY: integer);
var
  e, ePrior: PEdge;
  pp: PPolyPt;
begin
(*******************************************************************************
* Notes: Processing edges at scanline intersections (ie at the top or bottom   *
* of a scanbeam) needs to be done in multiple stages and in the correct order. *
* Firstly, edges forming a 'maxima' need to be processed and then removed.     *
* Next, 'intermediate' and 'maxima' horizontal edges are processed. Then edges *
* that intersect exactly at the top of the scanbeam are processed [%].         *
* Finally, new minima are added and any intersects they create are processed.  *
*******************************************************************************)

(*******************************************************************************
*     \                          /    /          \   /                         *
*      \   horizontal minima    /    /            \ /                          *
* { --  o======================#====o   --------   .     ------------------- } *
* {       horizontal maxima    .                   %  scanline intersect     } *
* { -- o=======================#===================#========o     ---------- } *
*      |                      /                   / \        \                 *
*      + maxima intersect    /                   /   \        \                *
*     /|\                   /                   /     \        \               *
*    / | \                 /                   /       \        \              *
*******************************************************************************)

  e := fActiveEdges;
  while assigned(e) do
  begin
    //1. process maxima, treating them as if they're 'bent' horizontal edges,
    //   but exclude maxima with horizontal edges. nb: e can't be a horizontal.
    if IsMaxima(e, topY) and (GetMaximaPair(e).dx <> horizontal) then
    begin
      //'e' might be removed from AEL, as may any following edges so ...
      ePrior := e.prevInAEL;
      DoMaxima(e, topY);
      if not assigned(ePrior) then
        e := fActiveEdges else
        e := ePrior.nextInAEL;
    end else
    begin
      //2. promote horizontal edges, otherwise update xcurr and ycurr ...
      if IsIntermediate(e, topY) and (e.nextInLML.dx = horizontal) then
      begin
        if (e.outIdx >= 0) then
          pp := AddPolyPt(e, Point(e.xtop, e.ytop));
        UpdateEdgeIntoAEL(e);
        AddEdgeToSEL(e);
      end else
      begin
        //this just simplifies horizontal processing ...
        e.xcurr := TopX(e, topY);
        e.ycurr := topY;
      end;
      e := e.nextInAEL;
    end;
  end;

  //3. Process horizontals at the top of the scanbeam ...
  ProcessHorizontals;

  if not assigned(fActiveEdges) then exit;

  //4. Promote intermediate vertices ...
  e := fActiveEdges;
  while assigned(e) do
  begin
    if IsIntermediate(e, topY) then
    begin
      if (e.outIdx >= 0) then AddPolyPt(e, Point(e.xtop, e.ytop));
      UpdateEdgeIntoAEL(e);
    end;
    e := e.nextInAEL;
  end;
end;
//------------------------------------------------------------------------------

function TClipper4.GetResult: TArrayOfArrayOfPoint;
var
  i,j,k,cnt: integer;
  p: PPolyPt;
begin
  GetHoleStates; //Get hole states *before* FixupOutPolygon()

  for i := 0 to fPolyPtList.Count -1 do
    if assigned(fPolyPtList[i]) then
      fPolyPtList[i] := FixupOutPolygon(fPolyPtList[i]);

  k := 0;
  setLength(result, fPolyPtList.Count);
  for i := 0 to fPolyPtList.Count -1 do
    if assigned(fPolyPtList[i]) then
    begin
      //make sure each polygons has at least 3 vertices ...
      cnt := 0;
      p := PPolyPt(fPolyPtList[i]);
      repeat
        p := p.next;
        inc(cnt);
      until (p = PPolyPt(fPolyPtList[i]));
      if (cnt < 3) then continue;

      if IsClockwise(p) = PHoleInfo(fHoleInfoList[i]).isHole then
        ReversePolyPtLinks(p);

      setLength(result[k], cnt);
      for j := 0 to cnt -1 do
      begin
        result[k][j].X := p.pt.X;
        result[k][j].Y := p.pt.Y;
        p := p.next;
      end;
      inc(k);
    end;
  setLength(result, k);
end;
//------------------------------------------------------------------------------

function TClipper4.FixupOutPolygon(outPoly: PPolyPt): PPolyPt;
var
  pp, tmp, lastOK: PPolyPt;
begin
  //FixupOutPolygon() - removes duplicate points and simplifies consecutive
  //parallel edges by removing the middle vertex.
  result := outPoly;
  if not assigned(outPoly) then exit;
  lastOK := nil;
  pp := outPoly;
  while true do
  begin
    if (pp.prev = pp) or (pp.next = pp.prev) then
    begin
      DisposePolyPts(pp);
      result := nil;
      exit;
    end;
    //test for duplicate points and for same slope ...
    if PointsEqual(pp.pt, pp.next.pt) or
      SlopesEqual(pp.prev.pt, pp.pt, pp.next.pt) then
    begin
      lastOK := nil;
      pp.prev.next := pp.next;
      pp.next.prev := pp.prev;
      tmp := pp;
      if pp = result then result := pp.prev;
      pp := pp.prev;
      dispose(tmp);
    end
    else if pp = lastOK then break
    else
    begin
      if not assigned(lastOK) then lastOK := pp;
      pp := pp.next;
    end;
  end;
end;
//------------------------------------------------------------------------------

function TClipper4.FixupIntersections: boolean;
var
  e1, e2: PEdge;
  int1, int2: PIntersectNode;
begin

  result := not assigned(fIntersectNodes.next);
  if result then exit;
  try
    CopyAELToSEL;
    int1 := fIntersectNodes;
    int2 := fIntersectNodes.next;
    while assigned(int2) do
    begin
      e1 := int1.edge1;
      if (e1.prevInSEL = int1.edge2) then e2 := e1.prevInSEL
      else if (e1.nextInSEL = int1.edge2) then e2 := e1.nextInSEL
      else
      begin
        //The current intersection is out of order, so try and swap it with
        //a subsequent intersection ...
        while assigned(int2) do
        begin
          if (int2.edge1.nextInSEL = int2.edge2) or
            (int2.edge1.prevInSEL = int2.edge2) then break
          else int2 := int2.next;
        end;
        if not assigned(int2) then exit; //oops!!!
        //found an intersect node that can be swapped ...
        SwapIntersectNodes(int1, int2);
        e1 := int1.edge1;
        e2 := int1.edge2;
      end;
      SwapPositionsInSEL(e1, e2);
      int1 := int1.next;
      int2 := int1.next;
    end;

    //finally, check the last intersection too ...
    result := (int1.edge1.prevInSEL = int1.edge2) or
      (int1.edge1.nextInSEL = int1.edge2);
  finally
    fSortedEdges := nil;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper4.SwapIntersectNodes(int1, int2: PIntersectNode);
var
  e1,e2: PEdge;
  p: TPoint;
begin
  with int1^ do
  begin
    e1 := edge1;
    edge1 := int2.edge1;
    e2 := edge2;
    edge2 := int2.edge2;
    p := pt;
    pt := int2.pt;
  end;
  with int2^ do
  begin
    edge1 := e1;
    edge2 := e2;
    pt := p;
  end;
end;
//------------------------------------------------------------------------------

function SortFunc(item1, item2: pointer): integer;
begin
  result := PHoleInfo(item2).pp.pt.Y - PHoleInfo(item1).pp.pt.Y;
end;
//------------------------------------------------------------------------------

procedure TClipper4.GetHoleStates;
var
  i, currY, ppX: integer;
  sortedHoleInfos: TList;
  t, t2, tracers, tracersEnd: PTracer;
  minPP, currPP, nextPP, tmpPP: PPolyPt;
  hole: boolean;
  dxF, dxP: double;

  function NextPolyPt(t: PTracer): PPolyPt;
  begin
    if t.dirF then result := t.pp.next else result := t.pp.prev;
  end;

  procedure AddTracer(p: PPolyPt; isForward: boolean);
  var
    t: PTracer;
  begin
    p.isDone := true;
    new(t);
    t.dirF := isForward;
    t.pp := p;
    if not assigned(tracers) then
    begin
      tracers := t;
      tracersEnd := t;
      t.next := nil;
      t.prev := nil;
    end else
    begin
      tracersEnd.next := t;
      t.prev := tracersEnd;
      t.next := nil;
      tracersEnd := t;
    end;
  end;

  procedure DeleteTracer(t: PTracer);
  begin
    if assigned(t.prev) then t.prev.next := t.next else tracers := t.next;
    if assigned(t.next) then t.next.prev := t.prev else tracersEnd := t.prev;
    dispose(t);
  end;

  procedure UpdateTracer(t: PTracer; currY: integer);
  var
    nextPP: PPolyPt;
  begin
    while true do
    begin
      t.pp.isDone := true;
      nextPP := NextPolyPt(t);
      if nextPP.pt.Y < currY -2 then exit;
      t.pp := nextPP;
      if t.pp.isDone then break;
    end;
    DeleteTracer(t);
  end;

  function IsClose(const pt1, pt2: TPoint; epsilon: integer = 2): boolean;
  begin
    result := (abs(pt1.X - pt2.X) + abs(pt1.Y - pt2.Y) <= epsilon);
  end;

begin
  if fHoleInfoList.Count = 0 then exit;
  sortedHoleInfos := TList.Create;
  try
    sortedHoleInfos.Capacity := fHoleInfoList.count;
    for i := 0 to fHoleInfoList.count -1 do
      sortedHoleInfos.Add(fHoleInfoList[i]);
    sortedHoleInfos.Sort(SortFunc);

    tracers := nil;

    with PHoleInfo(sortedHoleInfos[0])^ do
    begin
      while not assigned(fPolyPtList[idx]) do
        idx := PHoleInfo(fHoleInfoList[idx]).idx;
      PHoleInfo(fHoleInfoList[idx]).IsDone := true;
      PHoleInfo(fHoleInfoList[idx]).isHole := false;
      AddTracer(pp, true);
      AddTracer(pp, false);
    end;

    for i := 1 to sortedHoleInfos.count -1 do
      with PHoleInfo(sortedHoleInfos[i])^ do
      begin
        minPP := pp;
        currY := minPP.pt.Y;

        //first update tracers so they are current with currY ...
        t := tracers;
        while assigned(t) do
        begin
          t2 := t.next; //save t.next because t might be disposed
          UpdateTracer(t, currY);
          t := t2;
        end;

        //check we haven't already passed through this 'minima' ...
        if minPP.isDone then continue;

        //get the 'final' idx of the output polygon ...
        while not assigned(fPolyPtList[idx]) do
          idx := PHoleInfo(fHoleInfoList[idx]).idx;

        //if the hole state hasn't already been calculated, do it now ...
        if not PHoleInfo(fHoleInfoList[idx]).IsDone then
        begin
          hole := false;
          t := tracers;
          while assigned(t) do
          begin
            currPP := t.pp;
            nextPP := NextPolyPt(t);
            if (currPP.pt.X > minPP.pt.X +1) and (nextPP.pt.X > minPP.pt.X +1) then
              //do nothing
            else if (currPP.pt.X < minPP.pt.X -1) and (nextPP.pt.X < minPP.pt.X -1) then
              hole := not hole
            else
            begin
              ppX := TopX(currPP.pt, nextPP.pt, minPP.pt.Y);
              if ppX +2 < minPP.pt.X then hole := not hole
              else if ppX -2 > minPP.pt.X then //do nothing
              else
              begin
                //compare slopes of current edge with slope that bisects minima
                //being careful when other vertices are closely adjacent.
                //nb: while very close, this still isn't 100% ...
                if IsClose(nextPP.pt, minPP.pt) then currPP := nextPP;
                tmpPP := minPP.next;
                while (tmpPP.next <> minPP) and IsClose(minPP.pt, tmpPP.pt) do
                  tmpPP := tmpPP.next;
                dxF := GetDx(minPP.pt, tmpPP.pt);
                tmpPP := minPP.prev;
                while (tmpPP.prev <> minPP) and IsClose(minPP.pt, tmpPP.pt) do
                  tmpPP := tmpPP.prev;
                dxP := GetDx(minPP.pt, tmpPP.pt);
                if (dxF = horizontal) then
                begin
                  if minPP.next.pt.X > minPP.pt.X then hole := not hole;
                end
                else if (dxP = horizontal) then
                begin
                  if minPP.prev.pt.X > minPP.pt.X then hole := not hole;
                end
                else
                begin
                  dxF := (dxF + dxP)/2; //dx that bisects the minima vertex
                  if t.dirF then
                    dxP := GetDx(currPP.pt, currPP.next.pt) else
                    dxP := GetDx(currPP.pt, currPP.prev.pt);
                  if dxP > dxF then hole := not hole;
                end;
              end;
            end;
            t := t.next;
          end;
          PHoleInfo(fHoleInfoList[idx]).IsDone := true;
          PHoleInfo(fHoleInfoList[idx]).isHole := hole;
        end;
        AddTracer(minPP, true);
        AddTracer(minPP, false);
      end;
  finally
    //clean up ...
    t := tracers;
    while assigned(t) do
    begin
      t2 := t.next;
      Dispose(t);
      t := t2;
    end;
    sortedHoleInfos.free;
  end;
end;
//------------------------------------------------------------------------------

end.

unit Beziers;

(*******************************************************************************
*                                                                              *
* Author    :  Angus Johnson                                                   *
* Version   :  0.1                                                             *
* Date      :  13 June 2013                                                    *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2013                                         *
*                                                                              *
* License:                                                                     *
* Use, modification & distribution is subject to Boost Software License Ver 1. *
* http://www.boost.org/LICENSE_1_0.txt                                         *
*                                                                              *
*******************************************************************************)

//TODO - change binary tree to double-linked-list (halving space consumption)
//TODO - Add TCubicSpline, TQuadBezier & TQuadSpline classes

interface

uses
  Windows, Messages, SysUtils, Classes, Math, Clipper;

type
  PIntNode = ^TIntNode;
  TIntNode = record
    Val: Integer;
    Next: PIntNode;
    Prev: PIntNode;
  end;

  //nb: Specifications of TBezier class ...
  //Max. value for reference (passed to constructor) = 32,000
  //Max. number of bezier segments = 32,000
  TBezier = class
  private
    Reference: Word;
    SegmentList: TList;
    procedure GetSubBezierInternal(SegIdx: Integer;
      StartIdx, EndIdx: Int64; var IntList: PIntNode);
  public
    constructor Create(const CtrlPts: TPolygon; Ref: Word; Precision: Double = 0.5);
    destructor Destroy; override;
    //procedure Debug;
    function FlattenedPath: TPolygon;
    function GetSubBezier(startZ, endZ: Int64): TPolygon;
  end;

implementation

type
  TBezierType = (CubicBezier, CubicSpline, QuadBezier, QuadSpline);

  TSegment = class
  protected
    BezierType: TBezierType;
    Ref, Segment: Word;
    Index: Cardinal;
    Ctrls: array [0..3] of TDoublePoint;
    Childs: array [0..1] of TSegment;
    procedure GetFlattenedPath(var Path: TPolygon; Init: Boolean = False); virtual; abstract;
    procedure Find(Index, Msb: cardinal; out Segment: TSegment);
  public
    //procedure Debug;
    constructor Create(Ref, Seg, Idx: Cardinal); overload; virtual;
    destructor Destroy; override;
  end;

  TCubicBez = class(TSegment)
  protected
    procedure GetFlattenedPath(var Path: TPolygon; Init: Boolean = False); override;
  public
    constructor Create(const Pt1, Pt2, Pt3, Pt4: TDoublePoint;
      Ref, Seg, Idx: Cardinal; Precision: Double); overload;
  end;

//------------------------------------------------------------------------------
// Miscellaneous helper functions ...
//------------------------------------------------------------------------------

function InsertInt(InsertAfter: PIntNode; Val: Integer): PIntNode;
begin
  new(Result);
  Result.Val := Val;
  Result.Next := InsertAfter.Next;
  Result.Prev := InsertAfter;
  if assigned(InsertAfter.Next) then
    InsertAfter.Next.Prev := Result;
  InsertAfter.Next := Result;
end;
//------------------------------------------------------------------------------

function GetFirstIntNode(Current: PIntNode): PIntNode;
begin
  Result := Current;
  if not assigned(Result) then Exit;
  while assigned(Result.Prev) do
    Result := Result.Prev;
  //now skip the very first (dummy) node ...
  Result := Result.Next;
end;
//------------------------------------------------------------------------------

procedure DisposeIntNodes(IntNodes: PIntNode);
var
  IntNode: PIntNode;
begin
  if not assigned(IntNodes) then Exit;
  while assigned(IntNodes.Prev) do
    IntNodes := IntNodes.Prev;
  //OK, we're at the very first node.

  repeat
    IntNode := IntNodes;
    IntNodes := IntNodes.Next;
    Dispose(IntNode);
  until not assigned(IntNodes);
end;
//------------------------------------------------------------------------------

function DoublePoint(const Ip: TIntPoint): TDoublePoint; overload;
begin
  Result.X := Ip.X;
  Result.Y := Ip.Y;
end;
//------------------------------------------------------------------------------

function GetLeastSignificantBit(v: cardinal): cardinal; //index is zero based
const
   MulDeBruijnBitPosition: array[0..31] of integer =
    (0, 1, 28, 2, 29, 14, 24, 3, 30, 22, 20, 15, 25, 17, 4, 8,
    31, 27, 13, 23, 21, 19, 16, 7, 26, 12, 18, 6, 11, 5, 10, 9);
begin
  //http://stackoverflow.com/questions/757059/position-of-least-significant-bit-that-is-set
  Result := MulDeBruijnBitPosition[((v and -v) * $77CB531) shr 27];
end;
//------------------------------------------------------------------------------

function GetMostSignificantBit(v: cardinal): cardinal; //index is zero based
var
  i: cardinal;
const
  b: array [0..4] of cardinal = ($2, $C, $F0, $FF00, $FFFF0000);
  s: array [0..4] of cardinal = (1, 2, 4, 8, 16);
begin
  result := 0;
  for i := 4 downto 0 do
    if (v and b[i] <> 0) then
    begin
      v := v shr s[i];
      result := result or s[i];
    end;
end;
//------------------------------------------------------------------------------

function IsBitSet(val, index: cardinal): boolean;
begin
  result := val and (1 shl index) <> 0;
end;

//------------------------------------------------------------------------------
// TSegment methods ...
//------------------------------------------------------------------------------

constructor TSegment.Create(Ref, Seg, Idx: Cardinal);
begin
  self.Ref := Ref; Segment := Seg; Index := Idx;
  childs[0] := nil;
  childs[1] := nil;
end;
//------------------------------------------------------------------------------

destructor TSegment.Destroy;
begin
  FreeAndNil(childs[0]);
  FreeAndNil(childs[1]);
  inherited;
end;
//------------------------------------------------------------------------------

procedure TSegment.Find(Index, Msb: cardinal; out Segment: TSegment);
var
  Fill: Cardinal;
begin
  if not assigned(childs[0]) then
    Segment := Self
  else if (Msb = 0) then
    childs[0].Find(0, 0, Segment)
  else
  begin
    Index := Index and ((1 shl Msb) -1);
    Dec(Msb);
    Fill := 1 shl (Msb);
    if Index and Fill <> 0 then
      childs[1].Find(Index or Fill, Msb, Segment) else
      childs[0].Find(Index or Fill, Msb, Segment);
  end;
end;
//------------------------------------------------------------------------------

//procedure TSegment.Debug;
//var
//  ZVal: Int64;
//begin
//  if not assigned(childs[0]) then
//  begin
//    Int64Rec(ZVal).Lo := Index;
//    Int64Rec(ZVal).Hi := Ref shl 16 or Segment;
//      DebugUnit.Debug.LogFormatted('%1.0f, %1.0f, %16.16x',
//        [ctrls[3].X, ctrls[3].Y, ZVal]);
//    Exit;
//  end;
//  childs[0].Debug;
//  childs[1].Debug;
//end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// TCubicBez methods ...
//------------------------------------------------------------------------------

constructor TCubicBez.Create(const Pt1, Pt2, Pt3, Pt4: TDoublePoint;
  Ref, Seg, Idx: Cardinal; Precision: Double);
var
  p12, p23, p34, p123, p234, p1234: TDoublePoint;
const
  half = 0.5;
begin
  inherited Create(Ref, Seg, Idx);
  BezierType := CubicBezier;
  ctrls[0] := Pt1; ctrls[1] := Pt2; ctrls[2] := Pt3; ctrls[3] := Pt4;
  //assess curve flatness:
  //http://groups.google.com/group/comp.graphics.algorithms/tree/browse_frm/thread/d85ca902fdbd746e
  if abs(Pt1.x + Pt3.x - 2*Pt2.x) + abs(Pt2.x + Pt4.x - 2*Pt3.x) +
    abs(Pt1.y + Pt3.y - 2*Pt2.y) + abs(Pt2.y + Pt4.y - 2*Pt3.y) < Precision then
      Exit;

  //if not at maximum precision then (recursively) create sub-segments ...
  p12.X := (Pt1.X + Pt2.X) * half;
  p12.Y := (Pt1.Y + Pt2.Y) * half;
  p23.X := (Pt2.X + Pt3.X) * half;
  p23.Y := (Pt2.Y + Pt3.Y) * half;
  p34.X := (Pt3.X + Pt4.X) * half;
  p34.Y := (Pt3.Y + Pt4.Y) * half;
  p123.X := (p12.X + p23.X) * half;
  p123.Y := (p12.Y + p23.Y) * half;
  p234.X := (p23.X + p34.X) * half;
  p234.Y := (p23.Y + p34.Y) * half;
  p1234.X := (p123.X + p234.X) * half;
  p1234.Y := (p123.Y + p234.Y) * half;
  Idx := Idx shl 1;
  Childs[0] := TCubicBez.Create(Pt1, p12, p123, p1234, Ref, Seg, Idx, Precision);
  Childs[1] := TCubicBez.Create(p1234, p234, p34, Pt4, Ref, Seg, Idx +1, Precision);
end;
//------------------------------------------------------------------------------

procedure TCubicBez.GetFlattenedPath(var Path: TPolygon; Init: Boolean = False);
var
  Len: Integer;
  Z: Int64;
begin
  //nb. The format (high to low) of the 64bit Z value returned in the path ...
  //Typ  (2): any one of CubicBezier, CubicSpline, QuadBezier, QuadSpline
  //Ref (15): reference value passed to TBezier owner object
  //Seg (15): segment index since a bezier may consist of multiple segments
  //Idx (32): binary index to sub-segment containing control points
  if Init then
  begin
    Len := Length(Path);
    SetLength(Path, Len +1);
    Path[Len].X := Round(ctrls[0].X);
    Path[Len].Y := Round(ctrls[0].Y);
    Int64Rec(Z).Lo := Index;
    Int64Rec(Z).Hi := ord(BezierType) shr 30 + Ref shl 15 or Segment;
    Path[Len].Z := Z;
    Exit;
  end;

  if not assigned(childs[0]) then
  begin
    Len := Length(Path);
    SetLength(Path, Len +1);
    Path[Len].X := Round(ctrls[3].X);
    Path[Len].Y := Round(ctrls[3].Y);
    Int64Rec(Z).Lo := Index;
    Int64Rec(Z).Hi := ord(BezierType) shr 30 + Ref shl 15 or Segment;
    Path[Len].Z := Z;
    Exit;
  end;
  childs[0].GetFlattenedPath(Path);
  childs[1].GetFlattenedPath(Path);
end;

//------------------------------------------------------------------------------
// TBezier methods ...
//------------------------------------------------------------------------------

constructor TBezier.Create(const CtrlPts: TPolygon; Ref: Word; Precision: Double = 0.5);
var
  I, HighPts: Integer;
  CubicBez: TCubicBez;
begin
  Reference := Ref;
  if Precision <= 0.0 then Precision := 0.1;
  HighPts := High(CtrlPts);
  if (HighPts = 0) or (HighPts mod 3 <> 0) then
    raise Exception.Create('TBezier: invalid number of control points.');
  SegmentList := TList.Create;
  SegmentList.Capacity := HighPts div 3;
  CubicBez := TCubicBez.Create(
    DoublePoint(CtrlPts[0]),
    DoublePoint(CtrlPts[1]),
    DoublePoint(CtrlPts[2]),
    DoublePoint(CtrlPts[3]),
    Ref, 1, 1, Precision);
  SegmentList.Add(CubicBez);
  for I := 1 to (HighPts div 3) -1 do
  begin
    CubicBez :=
      TCubicBez.Create(
        DoublePoint(CtrlPts[I*3]),
        DoublePoint(CtrlPts[I*3+1]),
        DoublePoint(CtrlPts[I*3+2]),
        DoublePoint(CtrlPts[I*3+3]),
        Ref, I+1, 1, Precision);
    SegmentList.Add(CubicBez);
  end;
end;
//------------------------------------------------------------------------------

destructor TBezier.Destroy;
var
  I: Integer;
begin
  for I := 0 to SegmentList.Count -1 do
    TObject(SegmentList[I]).Free;
  SegmentList.Free;
  inherited;
end;
//------------------------------------------------------------------------------

//procedure TBezier.Debug;
//var
//  I: Integer;
//  ZVal: Int64;
//  CubicBez: TCubicBez;
//begin
//  if SegmentList.Count = 0 then Exit;
//  CubicBez := TCubicBez(SegmentList[0]);
//  with CubicBez do
//  begin
//    Int64Rec(ZVal).Lo := Index;
//    Int64Rec(ZVal).Hi := Ref shl 16 or Segment;
//    DebugUnit.Debug.LogFormatted('%1.0f, %1.0f, %16.16x',
//      [ctrls[0].X, ctrls[0].Y, ZVal]);
//  end;
//  for I := 0 to SegmentList.Count -1 do
//    TCubicBez(SegmentList[I]).Debug;
//end;
//------------------------------------------------------------------------------

function TBezier.FlattenedPath: TPolygon;
var
  I: Integer;
  Segment: TSegment;
begin
  Result := Nil;
  if SegmentList.Count = 0 then Exit;

  Segment := TSegment(SegmentList[0]);
  Segment.GetFlattenedPath(Result, True); //initializes the path
  for I := 0 to SegmentList.Count -1 do
    TSegment(SegmentList[I]).GetFlattenedPath(Result);
end;
//------------------------------------------------------------------------------

function TBezier.GetSubBezier(startZ, endZ: Int64): TPolygon;
var
  I, J, K, Seg1, Seg2, Cnt: Integer;
  IntList, IntCurrent: PIntNode;
  Segment: TSegment;
  Reversed: Boolean;
begin
  result := nil;

  //first, make sure the reference IDs match ...
  I := startZ shr 48;
  J := endZ shr 48;
  if (Reference <> Word(I)) or (I <> J) then Exit;

  Seg1 := (startZ shr 32 and $FFFF) -1;
  Seg2 := (endZ shr 32 and $FFFF) -1;
  startZ := startZ and $FFFFFFFF;
  endZ := endZ and $FFFFFFFF;

  //now make sure segment IDs are in range ...
  if (Seg1 < 0) or (Seg1 >= SegmentList.Count) or
    (Seg2 < 0) or (Seg2 >= SegmentList.Count) then Exit;

  //check orientation because it's much simpler to temporarily unreverse when
  //the startIdx and endIdx are reversed ...
  Reversed := (Seg1 > Seg2);
  if Reversed then
  begin
    I := Seg1;
    Seg1 := Seg2;
    Seg2 := I;
    I := startZ;
    startZ := endZ;
    endZ := I;
  end;

  //do further checks for reversal, in case reversal within a single segment ...
  if not Reversed and (Seg1 = Seg2) and
    (startZ <> 1) and (endZ <> 1) then //nb: idx == 1 is a special case
  begin
    I := GetMostSignificantBit(startZ);
    J := GetMostSignificantBit(endZ);
    K := Max(I, J);
    //nb: we must compare Node indexes at the same level ...
    I := startZ shl (K - I);
    J := endZ shl (K - J);
    if I > J then
    begin
      K := startZ;
      startZ := endZ;
      endZ := K;
      Reversed := True;
    end;
  end;

  Cnt := 0;
  while Seg1 <= Seg2 do
  begin
    IntList := nil;
    try
      //create a dummy first IntNode for the Int List ...
      New(IntList);
      IntList.Val := 0;
      IntList.Next := nil;
      IntList.Prev := nil;
      IntCurrent := IntList;

      if Seg1 <> Seg2 then
        GetSubBezierInternal(Seg1, startZ, 1, IntCurrent) else
        GetSubBezierInternal(Seg1, startZ, endZ, IntCurrent);

      IntCurrent := IntList.Next; //skip the dummy IntNode
      while assigned(IntCurrent) do
      begin
        Segment := TSegment(SegmentList[Seg1]);
        J := IntCurrent.Val;
        K := GetMostSignificantBit(J);
        Dec(K);
        while K >= 0 do
        begin
          if IsBitSet(J, K) then
            Segment := Segment.childs[1] else
            Segment := Segment.childs[0];
          if not assigned(Segment) then Exit; //must be incorrect index values!!!
          Dec(K);
        end;

        SetLength(Result, Cnt +4);
        for K := 0 to 3 do
        begin
          Result[Cnt +K].X := Round(Segment.ctrls[K].X);
          Result[Cnt +K].Y := Round(Segment.ctrls[K].Y);
        end;
        Inc(Cnt, 4);
        IntCurrent := IntCurrent.Next;
      end; //while assigned(IntCurrent);
    finally
      DisposeIntNodes(IntList);
    end;

    inc(Seg1);
    startZ := 1;
  end;

  if Reversed then
    Result := Clipper.ReversePolygon(Result);
end;
//------------------------------------------------------------------------------

procedure TBezier.GetSubBezierInternal(SegIdx: Integer;
  StartIdx, EndIdx: Int64; var IntList: PIntNode);
var
  Level, L1, L2: Cardinal;
  I, J: Integer;
  N1, N2: Cardinal;
begin
  //get the maximum level ...
  L1 := GetMostSignificantBit(StartIdx);
  L2 := GetMostSignificantBit(EndIdx);
  Level := Max(L1, L2);

  //reuse L2 as a marker projected onto the bottom level ...
  J := Level - L2;
  if J > 0 then
    L2 := EndIdx shl J + (1 shl J) - 1 else
    L2 := EndIdx;

  //if N1 is a right branch add it to IntList and increment N1 ...
  if (StartIdx = 1) then
  begin
    N1 := 1;
    for I := 1 to Level do
      N1 := N1 shl 1;
  end else
  begin
    N1 := StartIdx +1;
    if Odd(N1) then
    begin
      IntList := InsertInt(IntList, N1);
      Inc(N1);
    end;
  end;

  //if N2 is a left branch add it to IntList and decrement N2 ...
  if (EndIdx = 1) then
    N2 := L2 else
    N2 := EndIdx;
  if not Odd(N2) then
  begin
    InsertInt(IntList, N2);
    Dec(N2);
    Dec(L2);
  end;

  //now get blocks of nodes from the LEFT ...
  J := 1;
  repeat
    //while next level up then down-right doesn't exceed L2 do ...
    while not Odd(N1) and (((((N1 shr 1) + 1) shl J) -1) <= L2) do
    begin
      N1 := N1 shr 1; //go up a level
      Inc(J);
    end;
    IntList := InsertInt(IntList, N1);
    Inc(N1);
    I := GetMostSignificantBit(N1);
  until IsBitSet(N1, I-1) or ((((N1 shr 1) + 1) shl J) -1 > L2);

  L1 := N1 shl (J -1); //nb: the L1 marker is now just beyond N1's coverage

  //now get blocks of nodes from the RIGHT ...
  J := 1;
  if L1 < L2 then
    repeat
      while Odd(N2) and (((N2 shr 1) shl J) >= L1) do
      begin
        N2 := N2 shr 1; //go up a level
        Inc(J);
      end;
      InsertInt(IntList, N2);
      Dec(N2);
      I := GetMostSignificantBit(N2);
    until not IsBitSet(N2, I) or (N2 shl (J -1) < L1);
end;
//------------------------------------------------------------------------------

end.

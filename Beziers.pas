unit Beziers;

(*******************************************************************************
*                                                                              *
* Author    :  Angus Johnson                                                   *
* Version   :  0.1i                                                            *
* Date      :  14 June 2013                                                    *
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
  Windows, Messages, SysUtils, Classes, Math, clipper;

type

  //TBezierType: a parameter of TBezier's constructor
  TBezierType = (CubicBezier, CubicSpline, QuadBezier, QuadSpline);

  //IntNode: used internally only
  PIntNode = ^TIntNode;
  TIntNode = record
    Val: Integer;
    Next: PIntNode;
    Prev: PIntNode;
  end;

  //The TPolygon structure is defined in Clipper.pas ...
  //TPolygon = Array of TIntPoint;
  //TIntPoint = record X,Y,Z: Int64; end;

  //TBezier: Flattens poly-bezier curves, and later reconstructs them.
  //The FlattenedPath method stores data in the Z members of the returned
  //TPolygon structure and this is used for bezier reconstruction.
  //Any two Z values (of the IntPoints returned by the FlattenedPath method)
  //are sufficient to allow reconstruction of part or all of the original curve.

  TBezier = class
  private
    Reference : Integer;
    BezierType: TBezierType;
    //supports poly-beziers (ie before flattening) with up to 16,383 segments
    SegmentList: TList;
    procedure ReconstructInternal(SegIdx: Integer;
      StartIdx, EndIdx: Int64; var IntCurrent: PIntNode);
  public
    constructor Create(
      const CtrlPts: TPolygon;   //CtrlPts: Bezier control points
      BezType: TBezierType;      //CubicBezier, QuadBezier, etc ...
      Ref: Word;                 //Ref: user supplied identifier;
      Precision: Double = 0.5);  //Precision of flattened path
    destructor Destroy; override;
    function FlattenedPath: TPolygon;
    //Reconstruct: returns a list of Bezier control points using the
    //information provided in the startZ and endZ parameters (together with
    //the object's stored data) ...
    function Reconstruct(startZ, endZ: Int64): TPolygon; //Control points again.
  end;

implementation

type
  TSegment = class
  protected
    BezierType: TBezierType;
    Ref, Segment: Word;
    Index: Cardinal;
    Ctrls: array [0..3] of TDoublePoint;
    Childs: array [0..1] of TSegment;
    procedure GetFlattenedPath(var Path: TPolygon; var Cnt: Integer; Init: Boolean = False); virtual; abstract;
  public
    constructor Create(Ref, Seg, Idx: Cardinal); overload; virtual;
    destructor Destroy; override;
  end;

  TCubicBez = class(TSegment)
  protected
    procedure GetFlattenedPath(var Path: TPolygon; var Cnt: Integer; Init: Boolean = False); override;
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

  repeat
    IntNode := IntNodes;
    IntNodes := IntNodes.Next;
    Dispose(IntNode);
  until not assigned(IntNodes);
end;
//------------------------------------------------------------------------------

procedure AppendToPath(var Path: TPolygon; var Cnt: Integer; const Pt: TIntPoint);
const
  buffSize = 128;
begin
  if Cnt mod buffSize = 0 then
    SetLength(Path, Length(Path) +  buffSize);
  Path[Cnt] := Pt;
  Inc(Cnt);
end;
//------------------------------------------------------------------------------

function DoublePoint(const Ip: TIntPoint): TDoublePoint; overload;
begin
  Result.X := Ip.X;
  Result.Y := Ip.Y;
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

//nb. The format (high to low) of the 64bit Z value returned in the path ...
//Typ  (2): any one of CubicBezier, CubicSpline, QuadBezier, QuadSpline
//Seg (14): segment index since a bezier may consist of multiple segments
//Ref (16): reference value passed to TBezier owner object
//Idx (32): binary index to sub-segment containing control points

function MakeZ(BezierType: TBezierType; Seg, Ref, Idx: Integer): Int64; inline;
begin
  Int64Rec(Result).Lo := Idx;
  Int64Rec(Result).Hi := byte(BezierType) shl 30 + Seg shl 16 + Ref;
end;
//------------------------------------------------------------------------------

function UnMakeZ(ZVal: Int64;
  out BezierType: TBezierType; out Seg, Ref: Integer): Cardinal;
begin
  Result := Int64Rec(ZVal).Lo;
  BezierType := TBezierType(ZVal shr 62);
  Ref := Int64Rec(ZVal).Hi and $FFFF;
  Seg := Int64Rec(ZVal).Hi shr 16 and $3FFF -1; //convert segments to zero-base
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

procedure TCubicBez.GetFlattenedPath(var Path: TPolygon;
  var Cnt: Integer; Init: Boolean = False);
var
  Z: Int64;
begin
  if Init then
  begin
    Z := MakeZ(BezierType, Segment, Ref, Index);
    AppendToPath(Path, Cnt, IntPoint(Round(ctrls[0].X), Round(ctrls[0].Y), Z));
  end else if not assigned(childs[0]) then
  begin
    Z := MakeZ(BezierType, Segment, Ref, Index);
    AppendToPath(Path, Cnt, IntPoint(Round(ctrls[3].X), Round(ctrls[3].Y), Z));
  end else
  begin
    childs[0].GetFlattenedPath(Path, Cnt);
    childs[1].GetFlattenedPath(Path, Cnt);
  end;
end;

//------------------------------------------------------------------------------
// TBezier methods ...
//------------------------------------------------------------------------------

constructor TBezier.Create(const CtrlPts: TPolygon;
  BezType: TBezierType; Ref: Word; Precision: Double = 0.5);
var
  I, HighPts: Integer;
  CubicBez: TCubicBez;
begin
  BezierType := BezType;
  case BezType of
    CubicBezier: I := 3;
    CubicSpline: I := 2;
    QuadBezier: I := 2;
    else I := 1;
  end;
  HighPts := High(CtrlPts);
  if (HighPts = 0) or (HighPts mod I <> 0) then
    raise Exception.Create('TBezier: invalid number of control points.');

  Reference  := Ref;
  if Precision <= 0.0 then Precision := 0.1;

  SegmentList := TList.Create;
  SegmentList.Capacity := HighPts div 3;

  //now for each segment in the poly-bezier create a binary tree structure
  //and add it to SegmentList ...
  for I := 0 to (HighPts div 3) -1 do
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

function TBezier.FlattenedPath: TPolygon;
var
  I, Cnt: Integer;
  Segment: TSegment;
begin
  Result := Nil;
  if SegmentList.Count = 0 then Exit;

  Cnt := 0;
  Segment := TSegment(SegmentList[0]);
  Segment.GetFlattenedPath(Result, Cnt, True); //initializes the path
  for I := 0 to SegmentList.Count -1 do
    TSegment(SegmentList[I]).GetFlattenedPath(Result, Cnt);
  SetLength(Result, Cnt);
end;
//------------------------------------------------------------------------------

procedure AddCtrlPoints(Segment: TSegment;
  var CtrlPts: TPolygon; var currCnt: Integer);
var
  I: Integer;
const
  buffSize = 128;
begin
  if currCnt mod buffSize = 0 then
    SetLength(CtrlPts, Length(CtrlPts) + buffSize);
  for I := 0 to 3 do
  begin
    CtrlPts[currCnt].X := Round(Segment.ctrls[I].X);
    CtrlPts[currCnt].Y := Round(Segment.ctrls[I].Y);
    Inc(currCnt);
  end;
end;
//------------------------------------------------------------------------------

function TBezier.Reconstruct(startZ, endZ: Int64): TPolygon;
var
  I, J, K, Seg1, Seg2, Cnt: Integer;
  BezType1, BezType2: TBezierType;
  IntList, IntCurrent: PIntNode;
  Segment: TSegment;
  Reversed: Boolean;
begin
  result := nil;
  startZ := UnMakeZ(startZ, BezType1, Seg1, I);
  endZ   := UnMakeZ(endZ,   BezType2, Seg2, J);

  if (BezType1 <> BezierType) or (BezType1 <> BezType2) or
    (Reference <> I) or (I <> J) then Exit;

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
        ReconstructInternal(Seg1, startZ, 1, IntCurrent) else
        ReconstructInternal(Seg1, startZ, endZ, IntCurrent);

      //IntList now contains the indexes of one or a series of sub-segments
      //that together define part of or all of the original segment.
      //We now append these sub-segments to the new list of control points ...

      IntCurrent := IntList.Next; //nb: skips the dummy IntNode
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

        AddCtrlPoints(Segment, Result, Cnt);
        IntCurrent := IntCurrent.Next;
      end; //while assigned(IntCurrent);

    finally
      DisposeIntNodes(IntList);
    end;

    inc(Seg1);
    startZ := 1;
  end;
  SetLength(Result, Cnt);
  if Reversed then
    Result := Clipper.ReversePolygon(Result);
end;
//------------------------------------------------------------------------------

procedure TBezier.ReconstructInternal(SegIdx: Integer;
  StartIdx, EndIdx: Int64; var IntCurrent: PIntNode);
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


  if (StartIdx = 1) then
    //special case: ie goto the bottom left of the binary tree ...
    N1 := 1 shl level
  else
    //For any given Z value, its corresponding X & Y coords (created by
    //FlattenPath using De Casteljau's algorithm) refered to the ctrl[3] coords
    //of many tiny polybezier segments. Since ctrl[3] coords are identical to
    //ctrl[0] coords in the following node, we can safely increment StartIdx ...
    N1 := StartIdx +1;

  //if N2 is a left branch add it to IntList and decrement N2 ...
  if (EndIdx = 1) then
    N2 := L2 else
    N2 := EndIdx;

  //now get blocks of nodes from the LEFT ...
  J := 1;
  repeat
    //while next level up then down-right doesn't exceed L2 do ...
    while not Odd(N1) and (((((N1 shr 1) + 1) shl J) -1) <= L2) do
    begin
      N1 := N1 shr 1; //go up a level
      Inc(J);
    end;
    IntCurrent := InsertInt(IntCurrent, N1); //nb: updates IntCurrent
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
      InsertInt(IntCurrent, N2); //nb: doesn't update IntCurrent
      Dec(N2);
      I := GetMostSignificantBit(N2);
    until not IsBitSet(N2, I) or (N2 shl (J -1) < L1);
end;
//------------------------------------------------------------------------------

end.

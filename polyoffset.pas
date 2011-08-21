unit polyoffset;

(*******************************************************************************
*                                                                              *
* Author    :  Angus Johnson                                                   *
* Version   :  0.5                                                             *
* Date      :  22 August 2011                                                  *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2011                                         *
*                                                                              *
* License:                                                                     *
* Use, modification & distribution is subject to Boost Software License Ver 1. *
* http://www.boost.org/LICENSE_1_0.txt                                         *
*                                                                              *
*******************************************************************************)

interface

uses
  SysUtils, Types, Classes, Math, Clipper;

type
  TJoinType = (jtButt, jtMiter, jtSquare, jtRound);

//Pre-condition: outer polygons must be orientated clockwise,
//and inner 'hole' polygons must be orientated counter-clockwise.
function OffsetPolygons(const pts: TPolygons; const delta: double;
  JoinType: TJoinType = jtButt; MiterLimit: double = 0.0): TPolygons;

implementation

type
  TDoublePoint = record X, Y: double; end;
  TArrayOfDoublePoint = array of TDoublePoint;

const
  buffLength: integer = 128;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure AddPoint(var polygon: TPolygon; var idx: integer; const pt: TIntPoint);
var
  len: integer;
begin
  len := length(polygon);
  if idx = len then
    setlength(polygon, len + buffLength);
  polygon[idx] := pt;
  inc(idx);
end;
//------------------------------------------------------------------------------

function BuildArc(const pt: TIntPoint; a1, a2, r: double): TPolygon;
var
  i, N: Integer;
  a, da: double;
  Steps: Integer;
  S, C: Extended; //sin & cos
begin
  Steps := Max(6, Round(Sqrt(Abs(r)) * Abs(a2 - a1)));
  SetLength(Result, Steps);
  N := Steps - 1;
  da := (a2 - a1) / N;
  a := a1;
  for i := 0 to N do
  begin
    SinCos(a, S, C);
    Result[i].X := pt.X + Round(C * r);
    Result[i].Y := pt.Y + Round(S * r);
    a := a + da;
  end;
end;
//------------------------------------------------------------------------------

function GetUnitNormal(const pt1, pt2: TIntPoint): TDoublePoint;
var
  dx, dy, f: double;
begin
  dx := (pt2.X - pt1.X);
  dy := (pt2.Y - pt1.Y);

  if (dx = 0) and (dy = 0) then
  begin
    result.X := 0;
    result.Y := 0;
  end else
  begin
    f := 1 / Hypot(dx, dy);
    dx := dx * f;
    dy := dy * f;
  end;
  Result.X := dy;
  Result.Y := -dx;
end;
//------------------------------------------------------------------------------

function GetBounds(const a: TPolygons): TIntRect;
var
  i,j,len: integer;
const
  nullRect: TIntRect = (left:0;top:0;right:0;bottom:0);
begin
  len := length(a);
  i := 0;

  while (i < len) and (length(a[i]) = 0) do inc(i);
  if i = len then begin result := nullRect; exit; end;

  with result, a[i][0] do
  begin
    Left := X; Top := Y; Right := X; Bottom := Y;
  end;

  for i := i to len-1 do
    for j := 0 to high(a[i]) do
    begin
      if a[i][j].X < result.Left then result.Left := a[i][j].X
      else if a[i][j].X > result.Right then result.Right := a[i][j].X;
      if a[i][j].Y < result.Top then result.Top := a[i][j].Y
      else if a[i][j].Y > result.Bottom then result.Bottom := a[i][j].Y;
    end;
end;
//------------------------------------------------------------------------------

function OffsetPolygons(const pts: TPolygons; const delta: double;
  JoinType: TJoinType; MiterLimit: double): TPolygons;
var
  i, j, k, highI, len, out_len: integer;
  normals: TArrayOfDoublePoint;
  a1, a2, deltaSq, R, RMin: double;
  arc, outer: TPolygon;
  bounds: TIntRect;
  clipper: TClipper;
  pt1, pt2: TIntPoint;
  unitVector: TDoublePoint;

  procedure DoButt;
  begin
    AddPoint(result[i], out_len, pt1);
    AddPoint(result[i], out_len, pt2);
  end;

  procedure DoSquare;
  begin
    if ((normals[j].X*normals[k].Y-normals[k].X*normals[j].Y)*delta >= 0) then
    begin
      unitVector.X := -normals[j].Y;
      unitVector.Y := normals[j].X;
      pt1 := IntPoint(round(pt1.X + unitVector.X *delta),
        round(pt1.Y + unitVector.Y *delta));
      AddPoint(result[i], out_len, pt1);
      unitVector.X := -normals[j].Y;
      unitVector.Y := normals[j].X;
      pt2 := IntPoint(round(pt2.X + unitVector.X *delta),
        round(pt2.Y + unitVector.Y *delta));
      AddPoint(result[i], out_len, pt2);
    end else
    begin
      AddPoint(result[i], out_len, pt1);
      AddPoint(result[i], out_len, pt2);
    end;
  end;


  procedure DoMiter;
  begin
    R := 1 + (normals[j].X*normals[k].X + normals[j].Y*normals[k].Y);
    if (R < RMin) then
    begin
      DoSquare;
    end else
    begin
      R := delta / R;
      pt1 := IntPoint(round(pts[i][j].X + (normals[j].X + normals[k].X)*R),
        round(pts[i][j].Y + (normals[j].Y + normals[k].Y)*R));
      AddPoint(result[i], out_len, pt1);
    end;
  end;

  procedure DoRound;
  var
    m: integer;
  begin
    AddPoint(result[i], out_len, pt1);
    //round off reflex angles (ie > 180 deg) unless it's
    //almost flat (ie < 10deg angle).
    //cross product normals < 0 -> angle > 180 deg.
    //dot product normals == 1 -> no angle
    if j = 0 then k := highI else k := j -1;
    if ((normals[j].X*normals[k].Y-normals[k].X*normals[j].Y)*delta >= 0) and
       ((normals[k].X*normals[j].X+normals[k].Y*normals[j].Y) < 0.985) then
    begin
      a1 := ArcTan2(normals[j].Y, normals[j].X);
      a2 := ArcTan2(normals[k].Y, normals[k].X);
      if (delta > 0) and (a2 < a1) then a2 := a2 + pi*2
      else if (delta < 0) and (a2 > a1) then a2 := a2 - pi*2;
      arc := BuildArc(pts[i][j], a1, a2, delta);
      for m := 0 to high(arc) do
        AddPoint(result[i], out_len, arc[m]);
    end;
    AddPoint(result[i], out_len, pt2);
  end;

begin
  deltaSq := delta*delta;
  if MiterLimit = 0 then
    RMin := 0 else
    RMin := 2/sqr(MiterLimit);

  setLength(result, length(pts));
  for i := 0 to high(pts) do
  begin
    len := length(pts[i]);
    highI := len -1;

    //when 'shrinking' polygons, to minimize artefacts,
    //strip those that have an area < Sqr(delta) ...
    a1 := Area(pts[i]);
    if (delta < 0) then
    begin
      if (a1 > 0) and (a1 < deltaSq) then len := 1;
    end else
      if (a1 < 0) and (-a1 < deltaSq) then len := 1; //ie: a hole if area < 0

    //allow the 'expansion' of single lines and points ...
    if (len < 3) and (delta <= 0) then
    begin
      result[i] := nil;
      continue;
    end;

    if len = 1 then
    begin
      result[i] := BuildArc(pts[i][0], 0, 2*pi, delta);
      continue;
    end;

    //build normals ...
    setLength(normals, len);
    normals[0] := GetUnitNormal(pts[i][highI], pts[i][0]);
    for j := 1 to highI do
      normals[j] := GetUnitNormal(pts[i][j-1], pts[i][j]);

    out_len := 0;
    for j := 0 to highI do
    begin
      if j = highI then k := 0 else k := j +1;
      pt1.X := round(pts[i][j].X +delta *normals[j].X);
      pt1.Y := round(pts[i][j].Y +delta *normals[j].Y);
      pt2.X := round(pts[i][j].X +delta *normals[k].X);
      pt2.Y := round(pts[i][j].Y +delta *normals[k].Y);

      case JoinType of
        jtButt: DoButt;
        jtMiter: DoMiter;
        jtSquare: DoSquare;
        jtRound: DoRound;
      end;
    end;
    setLength(result[i], out_len);
  end;

  //finally, clean up untidy corners ...
  clipper := TClipper.Create;
  try
    clipper.AddPolygons(result, ptSubject);
    if delta > 0 then
    begin
      if not clipper.Execute(ctUnion, result, pftNonZero, pftNonZero) then
        result := nil;
    end else
    begin
      bounds := GetBounds(result);
      setlength(outer, 4);
      outer[0] := IntPoint(bounds.left-10, bounds.bottom+10);
      outer[1] := IntPoint(bounds.right+10, bounds.bottom+10);
      outer[2] := IntPoint(bounds.right+10, bounds.top-10);
      outer[3] := IntPoint(bounds.left-10, bounds.top-10);
      clipper.AddPolygon(outer, ptSubject);
      if clipper.Execute(ctUnion, result, pftNonZero, pftNonZero) then
      begin
        //delete the outer rectangle ...
        highI := high(result);
        for j := 1 to highI do result[j-1] := result[j];
        setlength(result, highI);
      end else
        result := nil;
    end;
  finally
    clipper.free;
  end;
end;
//------------------------------------------------------------------------------

end.
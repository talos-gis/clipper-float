/*******************************************************************************
*                                                                              *
* Author    :  Angus Johnson                                                   *
* Version   :  0.99 beta                                                       *
* Date      :  14 September 2013                                               *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2013                                         *
*                                                                              *
* License:                                                                     *
* Use, modification & distribution is subject to Boost Software License Ver 1. *
* http://www.boost.org/LICENSE_1_0.txt                                         *
*                                                                              *
*******************************************************************************/

using System;
using System.Collections.Generic;
using ClipperLib;

namespace CurvesLib
{

  using Path = List<IntPoint>;
  using Paths = List<List<IntPoint>>;

  public enum CurveType { CubicBezier, QuadBezier, Arc, Ellipse, Open, Closed, Unknown };
  public enum PathType { Open, Closed, Both };

  //------------------------------------------------------------------------------

  public class CurveList
  {
    internal const double DefaultPrecision = 0.5;
    private List<Curve> m_Curves = new List<Curve>();
    
    public int Count { get{return m_Curves.Count;} }

    //------------------------------------------------------------------------------

    public static void ClipCallback(IntPoint vert1, IntPoint vert2, ref IntPoint intersectPt)
    {
      if ((CurveType)((UInt64)vert1.Z >> 60 & 0x3) == CurveType.Arc)
      {
        intersectPt.Z = (Int64)((UInt64)vert1.Z & 0xFFFFFFFF00000000 | 
          ((UInt32)vert1.Z + (UInt32)vert2.Z) >> 1); //ie for arcs => average the bottom 32bit value
      }
      else intersectPt.Z = vert2.Z; //ie use the 'inside' vertex (vert1 is the outside vertex)
    }
    //------------------------------------------------------------------------------

    public CurveList(double precision = DefaultPrecision)
    {
      if (precision <= 0) Precision = DefaultPrecision;
      else Precision = precision;
    }
    //------------------------------------------------------------------------------

    public bool AddPath(Path ctrlPts, CurveType ct)
    {
      Curve curve = null;
      switch (ct)
      {
        case CurveType.QuadBezier:
          if (ctrlPts.Count > 2) curve = new QBezierCurve(ctrlPts, (ushort)m_Curves.Count, Precision); 
          break;
        case CurveType.CubicBezier:
          if (ctrlPts.Count > 3) curve = new CBezierCurve(ctrlPts, (ushort)m_Curves.Count, Precision);
          break;
        case CurveType.Arc:
          if (ctrlPts.Count > 2) curve = new ArcCurve(ctrlPts, (ushort)m_Curves.Count, Precision);
          break;
        case CurveType.Ellipse:
          if (ctrlPts.Count == 3) curve = new EllipseCurve(ctrlPts, (ushort)m_Curves.Count, Precision);
          break;
      }

      if (curve != null)
      {
        m_Curves.Add(curve);
        return true;
      }
      else return false;
    }
    //------------------------------------------------------------------------------

    public int AddPaths(Paths ctrlPts, CurveType ct)
    {
      int cnt = 0;
      foreach (Path p in ctrlPts)
        if (AddPath(p, ct)) cnt++;
      return cnt;
    }
    //------------------------------------------------------------------------------

    public void Clear()
    {
      m_Curves.Clear();
    }
    //------------------------------------------------------------------------------

    public double Precision { get; set; }
    //------------------------------------------------------------------------------

    public CurveType GetCurveType(int index)
    {
      if (index < 0 || index >= m_Curves.Count)
        throw new CurveException("CurveList: index out of range.");
      Curve c = m_Curves[index];
      return c.GetCurveType();
    }
    //------------------------------------------------------------------------------

    public CurveType GetCurveType(IntPoint pt)
    {
      CurveType ct;
      UInt16 seg, refId;
      Curve.UnMakeZ(pt.Z, out ct, out seg, out refId);
      return ct;
    }
    //------------------------------------------------------------------------------

    public Path GetCtrlPts(int index)
    {
      if (index < 0 || index >= m_Curves.Count)
        throw new CurveException("CurveList: index out of range.");
      Path result = new Path(m_Curves[index].path);
      return result;   
    }
    //------------------------------------------------------------------------------

    public Path GetFlattenedPath(int index)
    {
      if (index < 0 || index >= m_Curves.Count)
        throw new CurveException("CurveList: index out of range.");
      Path result = new Path(m_Curves[index].FlattenedPath());
      return result;
    }
    //------------------------------------------------------------------------------

    internal static bool CurveTypeInPathType(CurveType ct, PathType pt)
    {
      if (pt == PathType.Both) return true;
      switch (ct)
      {
        case CurveType.Arc:
        case CurveType.CubicBezier:
        case CurveType.QuadBezier:
        case CurveType.Open:
          return (pt == PathType.Open);
        case CurveType.Ellipse:
        case CurveType.Closed:
          return (pt == PathType.Closed);
        default: return false;
      }
    }
    //------------------------------------------------------------------------------

    public Paths GetFlattenedPaths(PathType pt = PathType.Both)
    {
      Paths result = new Paths(m_Curves.Count);
      if (m_Curves.Count > 0) 
        foreach (Curve c in m_Curves)
          if (CurveTypeInPathType(c.GetCurveType(), pt))
            result.Add(c.FlattenedPath());
      return result;
    }
    //------------------------------------------------------------------------------

    public static Path Flatten(Path ctrlPts, CurveType ct, 
      UInt16 refID = 0, double precision = DefaultPrecision)
    {
      Curve curve = null;
      switch (ct)
      {
        case CurveType.QuadBezier:
          if (ctrlPts.Count > 2) curve = new QBezierCurve(ctrlPts, refID, precision); 
          break;
        case CurveType.CubicBezier:
          if (ctrlPts.Count > 3) curve = new CBezierCurve(ctrlPts, refID, precision);
          break;
        case CurveType.Arc:
          if (ctrlPts.Count > 2) curve = new ArcCurve(ctrlPts, refID, precision);
          break;
      }
      if (curve != null)
        return curve.FlattenedPath();
      else
        return new Path();
    }
    //------------------------------------------------------------------------------

    public static Paths Flatten(Paths paths, CurveType ct, 
      UInt16 refID = 0, double precision = DefaultPrecision)
    {
      int len = paths.Count;
      Paths result = new Paths(len);
      if (len == 0) return result;
      foreach (Path p in paths)
      {
        Path pp = Flatten(p, ct, refID, precision);
        if (pp.Count > 0) result.Add(pp);
      }
      return result;
    }
    //------------------------------------------------------------------------------

    private static IntPoint MidPoint(IntPoint p1, IntPoint p2)
    {
      IntPoint result = new IntPoint((p1.X + p2.X) / 2, (p1.Y + p2.Y) / 2);
      return result;
    }
    //------------------------------------------------------------------------------

    public static Path CSplineToCBezier(Path cSpline)
    {
      int len = cSpline.Count;
      if (len < 4) return new Path();
      if (len % 2 != 0) len--;
      int i = (len / 2) - 1;
      Path result = new Path(i * 3 + 1);
      result.Add(cSpline[0]);
      result.Add(cSpline[1]);
      result.Add(cSpline[2]);
      i = 3;
      int lenMin1 = len - 1;
      while (i < lenMin1)
      {
        result.Add(MidPoint(cSpline[i - 1], cSpline[i]));
        result.Add(cSpline[i]);
        result.Add(cSpline[i + 1]);
        i += 2;
      }
      result.Add(cSpline[lenMin1]);
      return result;
    }
    //------------------------------------------------------------------------------

    public static Path QSplineToQBezier(Path qSpline)
    {
      int len = qSpline.Count;
      if (len < 3) return new Path();
      if (len % 2 == 0) len--;
      int i = len - 2;
      Path result = new Path(i * 2 + 1);
      result.Add(qSpline[0]);
      result.Add(qSpline[1]);
      i = 2;
      int lenMin1 = len - 1;
      while (i < lenMin1)
      {
        result.Add(MidPoint(qSpline[i - 1], qSpline[i]));
        result.Add(qSpline[i++]);
      }
      result.Add(qSpline[lenMin1]);
      return result;
    }
    //------------------------------------------------------------------------------

    public Path Reconstruct(IntPoint Pt1, IntPoint Pt2)
    {
      Path result = new Path();
      UInt16 seg, refId;
      CurveType curvetype;
      Curve.UnMakeZ(Pt1.Z, out curvetype, out seg, out refId); //nb: just need refId
      if (refId >= 0 && refId < m_Curves.Count)
        result = m_Curves[refId].Reconstruct(Pt1, Pt2);
      return result;       
    }
    //------------------------------------------------------------------------------

  }; //end CurveList
  //------------------------------------------------------------------------------

  internal abstract class Curve
  {
    internal const double half = 0.5;
    internal const double rad180 = Math.PI;
    internal const double rad90 = rad180 / 2;
    internal const double rad270 = rad90 * 3;
    internal const double rad360 = rad180 * 2;
    internal const double TwoPI = rad180 * 2;

    internal UInt16 reference;
    internal double precision = CurveList.DefaultPrecision;
    internal Path path = new Path(); //path of Control Pts
    //segments: ie supports poly-beziers (ie before flattening) with up to 4,096 segments 
    //internal List<Segment> segments = new List<Segment>();

    //nb. The format (high to low) of the 64bit Z value returned in the path ...
    //SOP  (1): StartOfPath Flag
    //Typ  (3): CurveType (CubicBezier, QuadBezier, Arc)
    //Seg (12): Bezier segment idx since there may be multiple (4095) segments
    //Ref (16): Reference value passed to TCurve owner object
    //Idx (32): Binary index to sub-segment containing control points
    internal static Int64 MakeZ(CurveType ct, UInt16 seg, UInt16 refId, UInt32 idx)
    {
      //nb: SOP flag (bit62) is set separately
      UInt32 hi = (UInt32)((UInt16)ct << 28 | seg << 16 | (refId + 1));
      return (Int64)((UInt64)hi << 32 | idx);
    }
    //------------------------------------------------------------------------------

    internal static UInt32 UnMakeZ(Int64 zval, 
      out CurveType ct, out UInt16 seg, out UInt16 refId)
    {
      UInt32 vals = (UInt32)((UInt64)zval >> 32); //the top 32 bits => vals
      ct = (CurveType)((vals >> 28) & 0x7);
      seg = (UInt16)((vals >> 16) & 0xFFF);
      refId = (UInt16)((vals & 0xFFFF) - 1);
      return (UInt32)(zval & 0xFFFFFFFF);
    }
    //------------------------------------------------------------------------------

    internal abstract CurveType GetCurveType();

    internal static Int64 Round(double value)
    {
      return value < 0 ? (Int64)(value - 0.5) : (Int64)(value + 0.5);
    }
    //------------------------------------------------------------------------------

    internal static void SwapInts(ref UInt32 val1, ref UInt32 val2)
    {
      UInt32 tmp = val1;
      val1 = val2;
      val2 = tmp;
    }
    //------------------------------------------------------------------------------

    internal static void SwapInts(ref UInt16 val1, ref UInt16 val2)
    {
      UInt16 tmp = val1;
      val1 = val2;
      val2 = tmp;
    }
    //------------------------------------------------------------------------------

    internal static void SwapIntPoints(ref IntPoint val1, ref IntPoint val2)
    {
      IntPoint tmp = val1;
      val1 = val2;
      val2 = tmp;
    }
    //------------------------------------------------------------------------------

    internal static void SwapDoublePoints(ref DoublePoint val1, ref DoublePoint val2)
    {
      DoublePoint tmp = val1;
      val1 = val2;
      val2 = tmp;
    }
    //------------------------------------------------------------------------------

    internal static void sincos(double angle, out double sinval, out double cosval)
    {
      sinval = Math.Sin(angle);
      cosval = Math.Cos(angle);
    }
    //------------------------------------------------------------------------------

    ~Curve() { Clear(); }
    //------------------------------------------------------------------------------

    internal virtual void Clear() { path.Clear(); }
    //------------------------------------------------------------------------------

    internal Curve(Path ctrlPts, UInt16 refID, double precision) //constructor
    {
      SetCtrlPoints(ctrlPts, refID, precision);
    }
    //------------------------------------------------------------------------------

    internal virtual void SetCtrlPoints(Path ctrlPts, UInt16 refID, double precision)
    {
      Clear();
      reference = refID;
      path = new Path(ctrlPts);
      if (precision > 0) this.precision = precision;
    }
    //------------------------------------------------------------------------------

    internal virtual Path FlattenedPath()
    {
      return new Path();
    }

    //------------------------------------------------------------------------------

    internal virtual Path Reconstruct(IntPoint startZ, IntPoint endZ)
    {
      return new Path();
    }
    //------------------------------------------------------------------------------

  }; //end Curve
  //------------------------------------------------------------------------------

  internal abstract class BezierCurve : Curve
  {
    //segments: ie supports poly-beziers (ie before flattening) with up to 16,383 segments 
    internal List<Segment> segments = new List<Segment>();
    //internal CurveType curvetype;

    internal class IntNode
    {
      internal int val;
      internal IntNode next;
      internal IntNode prev;
      internal IntNode(int val) { this.val = val; }
    }

    private IntNode InsertInt(IntNode insertAfter, int val)
    {
      IntNode result = new IntNode(val);
      result.next = insertAfter.next;
      result.prev = insertAfter;
      if (insertAfter.next != null)
        insertAfter.next.prev = result;
      insertAfter.next = result;
      return result;
    }
    //------------------------------------------------------------------------------

    private UInt32 GetMostSignificantBit(UInt32 v) //index is zero based
    {
      UInt32[] b = { 0x2, 0xC, 0xF0, 0xFF00, 0xFFFF0000 };
      Int32[] s = { 0x1, 0x2, 0x4, 0x8, 0x10 };
      Int32 result = 0;
      for (int i = 4; i >= 0; --i)
        if ((v & b[i]) != 0)
        {
          v = v >> s[i];
          result = result | s[i];
        }
      return (UInt32)result;
    }
    //------------------------------------------------------------------------------

    private static bool IsBitSet(UInt32 val, Int32 index)
    {
      return (val & (1 << (int)index)) != 0;
    }
    //------------------------------------------------------------------------------

    private static bool Odd(Int32 val)
    {
      return (val % 2) != 0;
    }
    //------------------------------------------------------------------------------

    private static bool Even(Int32 val)
    {
      return (val % 2) == 0;
    }
    //------------------------------------------------------------------------------

    internal class Segment
    {
      internal CurveType curvetype;
      internal UInt16 RefID;
      internal UInt16 SegID;
      internal UInt32 Index;
      internal DoublePoint[] Ctrls = new DoublePoint[4];
      internal Segment[] Childs = new Segment[2];
      internal Segment(UInt16 refID, UInt16 segID, UInt32 idx, CurveType curvetype)
      {
        this.RefID = refID;
        this.SegID = segID;
        this.Index = idx;
        this.curvetype = curvetype;
      }

      internal void GetFlattenedSeg(Path path, bool init)
      {
        if (init)
        {
          Int64 Z = MakeZ(curvetype, SegID, RefID, Index);
          path.Add(new IntPoint(Round(Ctrls[0].X), Round(Ctrls[0].Y), Z));
        }

        if (Childs[0] == null)
        {
          int CtrlIdx = (curvetype == CurveType.CubicBezier ? 3 : 2);
          Int64 Z = MakeZ(curvetype, SegID, RefID, Index);
          path.Add(new IntPoint(Round(Ctrls[CtrlIdx].X), Round(Ctrls[CtrlIdx].Y), Z));
        }
        else
        {
          Childs[0].GetFlattenedSeg(path, false);
          Childs[1].GetFlattenedSeg(path, false);
        }
      }

      internal void AddCtrlPtsToPath(Path ctrlPts)
      {
        int firstDelta = (ctrlPts.Count == 0 ? 0 : 1);
        switch (curvetype)
        {
          case CurveType.CubicBezier:
          case CurveType.Ellipse:
            for (int i = firstDelta; i < 4; ++i)
            {
              ctrlPts.Add(new IntPoint(
                Round(Ctrls[i].X), Round(Ctrls[i].Y)));
            }
            break;
          case CurveType.QuadBezier:
            for (int i = firstDelta; i < 3; ++i)
            {
              ctrlPts.Add(new IntPoint(
                Round(Ctrls[i].X), Round(Ctrls[i].Y)));
            }
            break;
        }
      }
    } //end Segment
    //------------------------------------------------------------------------------

    internal class CBezSegment : Segment
    {
      internal CBezSegment(DoublePoint pt1, DoublePoint pt2, DoublePoint pt3, DoublePoint pt4,
        UInt16 refID, UInt16 segID, UInt32 idx, double precision, CurveType ct)
        : base(refID, segID, idx, ct)
      {

        curvetype = ct;
        Ctrls[0] = pt1; Ctrls[1] = pt2; Ctrls[2] = pt3; Ctrls[3] = pt4;
        //assess curve flatness:
        //http://groups.google.com/group/comp.graphics.algorithms/tree/browse_frm/thread/d85ca902fdbd746e
        if (Math.Abs(pt1.X + pt3.X - 2 * pt2.X) + Math.Abs(pt2.X + pt4.X - 2 * pt3.X) +
          Math.Abs(pt1.Y + pt3.Y - 2 * pt2.Y) + Math.Abs(pt2.Y + pt4.Y - 2 * pt3.Y) < precision)
          return;

        //if not at maximum precision then (recursively) create sub-segments ...
        //, p23, p34, p123, p234, p1234;
        DoublePoint p12 = new DoublePoint((pt1.X + pt2.X) * half, (pt1.Y + pt2.Y) * half);
        DoublePoint p23 = new DoublePoint((pt2.X + pt3.X) * half, (pt2.Y + pt3.Y) * half);
        DoublePoint p34 = new DoublePoint((pt3.X + pt4.X) * half, (pt3.Y + pt4.Y) * half);
        DoublePoint p123 = new DoublePoint((p12.X + p23.X) * half, (p12.Y + p23.Y) * half);
        DoublePoint p234 = new DoublePoint((p23.X + p34.X) * half, (p23.Y + p34.Y) * half);
        DoublePoint p1234 = new DoublePoint((p123.X + p234.X) * half, (p123.Y + p234.Y) * half);
        idx = idx << 1;
        Childs[0] = new CBezSegment(pt1, p12, p123, p1234, refID, segID, idx, precision, ct);
        Childs[1] = new CBezSegment(p1234, p234, p34, pt4, refID, segID, idx + 1, precision, ct);
      } //end CubicBez constructor
    } //end CubicBez

    internal class QBezSegment : Segment
    {
      internal QBezSegment(DoublePoint pt1, DoublePoint pt2, DoublePoint pt3,
        UInt16 refID, UInt16 segID, UInt32 idx, double precision, CurveType ct)
        : base(refID, segID, idx, ct)
      {

        curvetype = ct;
        Ctrls[0] = pt1; Ctrls[1] = pt2; Ctrls[2] = pt3;
        //assess curve flatness:
        if (Math.Abs(pt1.X + pt3.X - 2 * pt2.X) + Math.Abs(pt1.Y + pt3.Y - 2 * pt2.Y) < precision) return;

        //if not at maximum precision then (recursively) create sub-segments ...
        //DoublePoint p12, p23, p123;
        DoublePoint p12 = new DoublePoint((pt1.X + pt2.X) * half, (pt1.Y + pt2.Y) * half);
        DoublePoint p23 = new DoublePoint((pt2.X + pt3.X) * half, (pt2.Y + pt3.Y) * half);
        DoublePoint p123 = new DoublePoint((p12.X + p23.X) * half, (p12.Y + p23.Y) * half);
        idx = idx << 1;
        Childs[0] = new QBezSegment(pt1, p12, p123, refID, segID, idx, precision, ct);
        Childs[1] = new QBezSegment(p123, p23, pt3, refID, segID, idx + 1, precision, ct);
      } //end QuadBez constructor
    } //end QuadBez
    //------------------------------------------------------------------------------

    internal BezierCurve(Path ctrlPts, UInt16 refID, double precision) :
      base(ctrlPts, refID, precision) { } //constructor
    //------------------------------------------------------------------------------

    internal override void Clear() { segments.Clear(); }
    //------------------------------------------------------------------------------

    internal override Path FlattenedPath()
    {
      Path path = new Path();
      if (segments.Count == 0) return path;
      for (int i = 0; i < segments.Count; i++)
        segments[i].GetFlattenedSeg(path, i == 0);
      IntPoint pt = path[0];
      pt.Z = (Int64)((UInt64)pt.Z | 0x8000000000000000); //StartOfPath flag
      path[0] = pt;
      return path;
    }
    //------------------------------------------------------------------------------

    internal override Path Reconstruct(IntPoint pt1, IntPoint pt2)
    {
      Path out_poly = new Path();
      if (pt2 == pt1) return out_poly;

      CurveType bt1, bt2;
      UInt16 seg1, seg2;
      UInt16 ref1, ref2;
      UInt32 pt1Z = UnMakeZ(pt1.Z, out bt1, out seg1, out ref1);
      UInt32 pt2Z = UnMakeZ(pt2.Z, out bt2, out seg2, out ref2);

      if (bt1 != GetCurveType() || bt1 != bt2 ||
        ref1 != reference || ref1 != ref2) return out_poly;

      if (seg1 >= segments.Count || seg2 >= segments.Count) return out_poly;

      if (pt1Z == pt2Z && seg1 == seg2)
      {
        //they're almost the same point so return a very simple bezier ...
        out_poly.Add(pt1);
        out_poly.Add(pt1);
        out_poly.Add(pt2);
        out_poly.Add(pt2);
        return out_poly;
      }

      if (((UInt64)pt2.Z & 0x8000000000000000) != 0 || seg1 > seg2)
      {
        SwapIntPoints(ref pt1, ref pt2);
        SwapInts(ref seg1, ref seg2);
        SwapInts(ref pt1Z, ref pt2Z);
      }

      //do further checks for reversal, in case reversal within a single segment ...
      if (seg1 == seg2 && pt1Z != 1 && pt2Z != 1)
      {
        UInt32 i = GetMostSignificantBit(pt1Z);
        UInt32 j = GetMostSignificantBit(pt2Z);
        UInt32 k = Math.Max(i, j);
        //nb: we must compare Node indexes at the same level ...
        i = pt1Z << (int)(k - i);
        j = pt2Z << (int)(k - j);
        if (i > j)
        {
          SwapIntPoints(ref pt1, ref pt2);
          SwapInts(ref pt1Z, ref pt2Z);
        }
      }

      while (seg1 <= seg2)
      {
        //create a dummy first IntNode for the Int List ...
        IntNode intList = new IntNode(0);
        IntNode intCurr = intList;

        if (seg1 != seg2)
          BezierReconstruct(seg1, pt1Z, 1, intCurr);
        else
          BezierReconstruct(seg1, pt1Z, pt2Z, intCurr);

        //IntList now contains the indexes of one or a series of sub-segments
        //that together define part of or the whole of the original segment.
        //We now append these sub-segments to the new list of control points ...

        intCurr = intList.next; //nb: skips the dummy IntNode
        while (intCurr != null)
        {
          Segment s = segments[seg1];
          UInt32 j = (UInt32)intCurr.val;
          Int32 k = (Int32)GetMostSignificantBit(j) - 1;
          while (k >= 0)
          {
            if (s.Childs[0] == null) break;
            if (IsBitSet(j, k--))
              s = s.Childs[1];
            else
              s = s.Childs[0];
          }
          s.AddCtrlPtsToPath(out_poly);
          intCurr = intCurr.next;
        } //while 

        intList = null;
        seg1++;
        pt1Z = 1;
      }
      //'adjust' the coords of the start and end points ...
      //nb: this isn't a perfect solution but it'll be so close as to not matter.
      out_poly[0] = pt1;
      out_poly[out_poly.Count - 1] = pt2;
      return out_poly;
    }
    //------------------------------------------------------------------------------

    internal void BezierReconstruct(UInt16 segIdx, UInt32 startIdx, UInt32 endIdx, IntNode intCurr)
    {
      //get the maximum level ...
      UInt32 L1 = GetMostSignificantBit(startIdx);
      UInt32 L2 = GetMostSignificantBit(endIdx);
      UInt32 Level = Math.Max(L1, L2);

      if (Level == 0)
      {
        InsertInt(intCurr, 1);
        return;
      }

      int L, R;
      //Right marker (R): EndIdx projected onto the bottom level ...
      if (endIdx == 1)
      {
        R = (1 << (int)((Level + 1))) - 1;
      }
      else
      {
        int k = (int)(Level - L2);
        R = ((int)endIdx << k) + (1 << k) - 1;
      }

      if (startIdx == 1) //special case
      {
        //Left marker (L) is bottom left of the binary tree ...
        L = (1 << (int)Level);
        L1 = Level;
      }
      else
      {
        //For any given Z value, its corresponding X & Y coords (created by
        //FlattenPath using De Casteljau's algorithm) refered to the ctrl[3] coords
        //of many tiny polybezier segments. Since ctrl[3] coords are identical to
        //ctrl[0] coords in the following node, we can safely increment StartIdx ...
        L = (int)startIdx + 1;
        if (L == (1 << (int)(Level + 1))) return; //loops around tree so already at the end
      }

      //now get blocks of nodes from the LEFT ...
      int j = (int)(Level - L1);
      do
      {
        //while next level up then down-right doesn't exceed L2 do ...
        while (Even(L) && ((L << j) + (1 << (j + 1)) - 1 <= R))
        {
          L = (L >> 1); //go up a level
          j++;
        }
        intCurr = InsertInt(intCurr, L); //nb: updates IntCurrent
        L++;
      } while (L != (3 << (int)(Level - j - 1)) && //ie crosses the ditch in the middle
        (L << j) + (1 << j) < R);      //or L is now over or to the right of R

      L = (L << j);

      //now get blocks of nodes from the RIGHT ...
      j = 0;
      if (R >= L)
        do
        {
          while (Odd(R) && ((R - 1) << j >= L))
          {
            R = R >> 1; //go up a level
            j++;
          }
          InsertInt(intCurr, R); //nb: doesn't update IntCurrent
          R--;
        } while (R != (3 << (int)(Level - j)) - 1 && ((R << j) > L));

    }
    //------------------------------------------------------------------------------

  }; //end BezierCurve
  //------------------------------------------------------------------------------

  internal class CBezierCurve : BezierCurve
  {

    internal CBezierCurve(Path ctrlPts, UInt16 refID, double precision) :
      base(ctrlPts, refID, precision) { } //constructor
    //------------------------------------------------------------------------------

    internal override CurveType GetCurveType() { return CurveType.CubicBezier; }

    internal override void SetCtrlPoints(Path ctrlPts, UInt16 refID, double precision)
    {
      int highpts = ctrlPts.Count - 1;
      if (highpts < 3)  throw new CurveException("CubicBezier: insufficient control points.");
      else highpts -= highpts % 3;
      base.SetCtrlPoints(ctrlPts, refID, precision);
      for (UInt16 i = 0; i < ((UInt16)highpts / 3); ++i)
      {
        CBezSegment s = new CBezSegment(
                    new DoublePoint(ctrlPts[i * 3]),
                    new DoublePoint(ctrlPts[i * 3 + 1]),
                    new DoublePoint(ctrlPts[i * 3 + 2]),
                    new DoublePoint(ctrlPts[i * 3 + 3]),
                    refID, i, 1, precision, CurveType.CubicBezier);
        segments.Add(s);
      }
    }
    //------------------------------------------------------------------------------

  }; //end CBezierCurve
  //------------------------------------------------------------------------------

  internal class QBezierCurve : BezierCurve
  {

    internal QBezierCurve(Path ctrlPts, UInt16 refID, double precision) :
      base(ctrlPts, refID, precision) { } //constructor

    internal override CurveType GetCurveType() { return CurveType.QuadBezier; }

    internal override void SetCtrlPoints(Path ctrlPts, UInt16 refID, double precision)
    {
      int highpts = ctrlPts.Count - 1;
      if (highpts < 2) throw new CurveException("QuadBezier: insufficient control points.");
      else highpts -= highpts % 2;
      base.SetCtrlPoints(ctrlPts, refID, precision);
      for (UInt16 i = 0; i < ((UInt16)highpts / 2); ++i)
      {
        QBezSegment s = new QBezSegment(
                          new DoublePoint(ctrlPts[i * 2]),
                          new DoublePoint(ctrlPts[i * 2 + 1]),
                          new DoublePoint(ctrlPts[i * 2 + 2]),
                          refID, i, 1, precision, CurveType.QuadBezier);
        segments.Add(s);
      }
    }
    //------------------------------------------------------------------------------

  }; //end QBezierCurve
  //------------------------------------------------------------------------------

  internal class ArcCurve : Curve
  {
    private static double GetAngle(DoublePoint startPt, DoublePoint endPt)
    {
      double result;
      DoublePoint relEndPt = new DoublePoint(endPt.X - startPt.X, endPt.Y - startPt.Y);
      if (relEndPt.X == 0)
      {
        if (relEndPt.Y > 0) result = rad270; else result = rad90;
      }
      else
      {
        result = Math.Atan2(-relEndPt.Y, relEndPt.X);
        if (result < 0) result += rad360;
      }
      return result;
    }
    //------------------------------------------------------------------------------

    private static double GetBisectingAngle(double angle1, double angle2, bool isClockwise)
    {
      if (isClockwise && angle2 >= angle1) angle2 -= TwoPI;
      else if (!isClockwise && angle2 <= angle1) angle2 += TwoPI;
      double result = (angle1 + angle2) / 2;
      if (result < 0) result += TwoPI;
      if (result > TwoPI) result -= TwoPI;
      return result;
    }
    //------------------------------------------------------------------------------

    private static DoublePoint GetPointFromOrigin(DoublePoint origin, double radius, double angle)
    {
      double asin, acos;
      sincos(angle, out asin, out acos);
      return new DoublePoint(radius * acos + origin.X, -radius * asin + origin.Y);
    }
    //------------------------------------------------------------------------------

    private static bool RightTurning(DoublePoint dp1, DoublePoint dp2, DoublePoint dp3)
    {
      double dx1 = dp2.X - dp1.X;
      double dy1 = dp2.Y - dp1.Y;
      double dx2 = dp3.X - dp2.X;
      double dy2 = dp3.Y - dp2.Y;
      return ((dx1 * dy2) - (dx2 * dy1)) > 0;
    }
    //------------------------------------------------------------------------------

    private static bool CircleFrom3Points(DoublePoint dp1, DoublePoint dp2, DoublePoint dp3, 
      out DoublePoint origin, out double radius)
    {
      //logic: A line perpendicular to a chord that passes through the chord's midpoint 
      //must also pass through the origin. Therefore given 2 chords we can find the origin.
      origin = new DoublePoint();
      radius = 0.0;
      double m1, m2, mp1x, mp1y, mp2x, mp2y;
      if (dp1.Y == dp2.Y) 
      {
        //test for collinear points ...
        if ((dp3.Y-dp1.Y)*(dp2.X-dp3.X) == (dp3.X-dp1.X)*(dp3.Y-dp2.Y)) return false;
        m1 = (dp3.X-dp1.X) / (dp1.Y-dp3.Y); //nb: inverse slopes
        m2 = (dp2.X - dp3.X) / (dp3.Y - dp2.Y);
        mp1x = (dp1.X + dp3.X) / 2;
        mp1y = (dp1.Y + dp3.Y) / 2;
        mp2x = (dp3.X + dp2.X) / 2;
        mp2y = (dp3.Y + dp2.Y) / 2;
      }
      else if (dp2.Y == dp3.Y) 
      {
        if ((dp1.Y-dp2.Y)*(dp1.X-dp3.X) == (dp2.X-dp1.X)*(dp3.Y-dp1.Y)) return false;
        m1 = (dp2.X - dp1.X) / (dp1.Y - dp2.Y); //nb: inverse slopes
        m2 = (dp1.X - dp3.X) / (dp3.Y - dp1.Y);
        mp1x = (dp1.X + dp2.X) / 2;
        mp1y = (dp1.Y + dp2.Y) / 2;
        mp2x = (dp3.X + dp1.X) / 2;
        mp2y = (dp3.Y + dp1.Y) / 2;
      }
      else //use 1-2, 2-3
      {
        if ((dp1.Y-dp2.Y)*(dp2.X-dp3.X) == (dp2.X-dp1.X)*(dp3.Y-dp2.Y)) return false;
        m1 = (dp2.X - dp1.X) / (dp1.Y - dp2.Y); //nb: inverse slopes
        m2 = (dp2.X - dp3.X) / (dp3.Y - dp2.Y);
        mp1x = (dp1.X + dp2.X) / 2;
        mp1y = (dp1.Y + dp2.Y) / 2;
        mp2x = (dp3.X + dp2.X) / 2;
        mp2y = (dp3.Y + dp2.Y) / 2;
      }
      double b1 = mp1y - mp1x * m1 ;
      double b2 = mp2y - mp2x * m2;
      origin.X = (b2 - b1) / (m1 - m2);
      origin.Y = m1 * origin.X + b1;
      radius = Math.Sqrt((dp1.X - origin.X) * (dp1.X - origin.X) +
        (dp1.Y - origin.Y) * (dp1.Y - origin.Y));
      return true;
    }
    //---------------------------------------------------------------------------

    internal ArcCurve(Path ctrlPts, UInt16 refID, double precision) :
      base(ctrlPts, refID, precision) { } //constructor

    internal override CurveType GetCurveType() { return CurveType.Arc; }

    internal override void SetCtrlPoints(Path ctrlPts, UInt16 refID, double precision)
    {
      int highpts = ctrlPts.Count - 1;
      if (highpts < 2) throw new CurveException("Arc: insufficient control points.");
      else highpts -= highpts % 2;
      base.SetCtrlPoints(ctrlPts, refID, precision);
    }
    //------------------------------------------------------------------------------

    internal override Path FlattenedPath()
    {
      Path result = new Path();
      DoublePoint p1 = new DoublePoint(path[0]);
      for (int i = 1; i < path.Count - 1; i += 2)
      {
        DoublePoint p2 = new DoublePoint(path[i]);
        DoublePoint p3 = new DoublePoint(path[i + 1]); 
        DoublePoint origin;
        double radius;

        if (!CircleFrom3Points(p1, p2, p3, out origin, out radius))
          return new Path(); //oops - p1, p2 & p3 must be collinear
        bool isClockwise = RightTurning(p1, p2, p3);
        double a1 = GetAngle(p1, origin);
        double a3 = GetAngle(p3, origin);
        double frac = Math.Abs(a3 - a1) / (2 * Math.PI);
        if (isClockwise == (a3 >= a1)) frac = 1 - frac;

        int steps = (int)Round(Math.PI / Math.Acos(1 - precision / radius)) + 1;
        if (steps < 2) steps = 2;
        double asin, acos, angle = frac * 2 * Math.PI / steps;
        if (!isClockwise) angle = -angle;
        sincos(angle, out asin, out acos);
        double x = ((double)p1.X - origin.X);
        double y = ((double)p1.Y - origin.Y);
        int j = 0;
        for ( ; j < steps; j++)
        {
          result.Add(new IntPoint(
            Round(origin.X + x),
            Round(origin.Y + y),
            MakeZ(CurveType.Arc, (UInt16)((i - 1) / 2), reference, (UInt16)(j*2))));
          double x2 = x;
          x = x * acos - asin * y;
          y = x2 * asin + y * acos;
        }
        j++;
        IntPoint endPt = new IntPoint(path[i +1].X, path[i +1].Y,
          MakeZ(CurveType.Arc, (UInt16)((i - 1) / 2), reference, (UInt16)(j*2)));
        result.Add(endPt);
        p1 = p3;
      }
      return result;
    }
    //------------------------------------------------------------------------------

    private static double NormalizeAngle(double angle)
    {
      if (angle >= rad360)
        while (angle >= rad360) angle -= rad360;
      else if (angle < 0)
        while (angle < 0) angle += rad360;
      return angle;
    }
    //------------------------------------------------------------------------------

    private static bool IsCollinear(DoublePoint pt1, DoublePoint pt2, DoublePoint pt3)
    {
      //cross product...
      Int64 dx1 = (Int64)(pt2.X - pt1.X);
      Int64 dy1 = (Int64)(pt2.Y - pt1.Y);
      Int64 dx2 = (Int64)(pt3.X - pt2.X);
      Int64 dy2 = (Int64)(pt3.Y - pt2.Y);
      return ((dx1 * dy2) - (dx2 * dy1)) == 0;
    }
    //------------------------------------------------------------------------------

    internal override Path Reconstruct(IntPoint pt1, IntPoint pt2)
    {
      Path result = new Path();
      if (pt2 == pt1) return result;

      int arcCount = (path.Count - 1) / 2;
      CurveType ct1, ct2;
      UInt16 seg1, seg2;
      UInt16 ref1, ref2;
      UInt32 z1 = UnMakeZ(pt1.Z, out ct1, out seg1, out ref1);
      UInt32 z2 = UnMakeZ(pt2.Z, out ct2, out seg2, out ref2);

      if (ct1 != CurveType.Arc || ct1 != ct2 ||
        ref1 != reference || ref1 != ref2 ||
        seg1 >= arcCount || seg2 >= arcCount) return result;

      if (seg1 > seg2 ||(seg1 == seg2 && z1 > z2)) 
      {
        SwapInts(ref seg1, ref seg2);
        SwapIntPoints(ref pt1, ref pt2);
      }
      else if (seg1 == seg2 && z1 == z2)
        return result;
      
      int idx1 = seg1 * 2, idx2 = seg2 * 2 + 2;
      result.Capacity = idx2 - idx1 + 1;

      for (int j = seg1; j <= seg2; j++)
      {
        if ((j == seg1 && pt1 != path[j * 2])
          || (j == seg2 && pt2 != path[j * 2 + 2]))
        {
          DoublePoint origin = new DoublePoint();
          double radius;
          DoublePoint dp1 = new DoublePoint(path[j * 2]);
          DoublePoint dp2 = new DoublePoint(path[j * 2 + 1]);
          DoublePoint dp3 = new DoublePoint(path[j * 2 + 2]);
          //get the origin, radius and orientation of each arc ...
          if (!CircleFrom3Points(dp1, dp2, dp3, out origin, out radius))
            return result; //not an arc as points collinear. (should never happen)
          bool isClockwise = RightTurning(dp1, dp2, dp3);

          if (j == seg1) dp2 = new DoublePoint(pt1);
          else dp2 = dp1;
          if (j == seg2) dp3 = new DoublePoint(pt2);
          //now get the new mid-arc point ...
          double a1 = GetAngle(origin, dp2);
          double a3 = GetAngle(origin, dp3);
          double a2 = NormalizeAngle((a1 + a3) / 2);
          //now check that this new point is oriented correctly ...
          DoublePoint midpoint = GetPointFromOrigin(origin, radius, a2);
          //todo - collinearity test ==> 'near' collinearity test
          if (Math.Abs(a1 - a2) > 0.2 &&
            RightTurning(dp2, midpoint, dp3) != isClockwise &&
            !IsCollinear(dp2, midpoint, dp3))
          {
            midpoint = GetPointFromOrigin(origin, radius, NormalizeAngle(a2 + rad180));
          }
          result.Add(new IntPoint(dp2.X, dp2.Y));
          result.Add(new IntPoint(midpoint.X, midpoint.Y));
          if (j == seg2 || (z2 == 0 && j == seg2 - 1))
          {
            result.Add(new IntPoint(dp3.X, dp3.Y));
            break;
          }
        }
        else
        {
          result.Add(path[j * 2]);
          result.Add(path[j * 2 + 1]);
          if (j == seg2) result.Add(path[j * 2 + 2]);
        }
      }
      return result;
    }
    //------------------------------------------------------------------------------

  }; //end ArcCurve
  //------------------------------------------------------------------------------

  internal class EllipseCurve : BezierCurve
  {

    internal EllipseCurve(Path ctrlPts, UInt16 refID, double precision) :
      base(ctrlPts, refID, precision) { } //constructor

    internal override CurveType GetCurveType() { return CurveType.Ellipse; }

    internal override void SetCtrlPoints(Path ctrlPts, UInt16 refID, double precision)
    {
      int highpts = ctrlPts.Count - 1;
      if (highpts != 2) throw new CurveException("Ellipse: requires 3 control points.");

      DoublePoint center;
      double rx, ry, angle;
      if (!GetEllipseFrom3Points(ctrlPts[0], ctrlPts[1], ctrlPts[2],
        out center, out rx, out ry, out angle))
          throw new CurveException("Ellipse: invalid control points.");

      //Magic constant = 4/3*(sqrt(2)-1) = 0.5522847498308
      const double offset = 0.5522847498308;
      double offx = rx * offset, offy = ry * offset;
      Path pts = new Path(13);
      pts.Add(new IntPoint(center.X + rx, center.Y));
      pts.Add(new IntPoint(center.X + rx, center.Y - offy));
      pts.Add(new IntPoint(center.X + offx, center.Y - ry));
      pts.Add(new IntPoint(center.X, center.Y - ry));
      pts.Add(new IntPoint(center.X - offx, center.Y - ry));
      pts.Add(new IntPoint(center.X - rx, center.Y - offy));
      pts.Add(new IntPoint(center.X - rx, center.Y));
      pts.Add(new IntPoint(center.X - rx, center.Y + offy));
      pts.Add(new IntPoint(center.X - offx, center.Y + ry));
      pts.Add(new IntPoint(center.X, center.Y + ry));
      pts.Add(new IntPoint(center.X + offx, center.Y + ry));
      pts.Add(new IntPoint(center.X + rx, center.Y + offy));
      pts.Add(new IntPoint(center.X + rx, center.Y));
      if (angle != 0) Rotate(pts, center, angle);
      
      base.SetCtrlPoints(pts, refID, precision);
      
      for (UInt16 i = 0; i < 4; ++i)
      {
        CBezSegment s = new CBezSegment(
                    new DoublePoint(pts[i * 3]),
                    new DoublePoint(pts[i * 3 + 1]),
                    new DoublePoint(pts[i * 3 + 2]),
                    new DoublePoint(pts[i * 3 + 3]),
                    refID, i, 1, precision, CurveType.Ellipse);
        segments.Add(s);
      }
    }
    //------------------------------------------------------------------------------

    private DoublePoint ClosestPointOnLine(DoublePoint pt, DoublePoint linePt1, DoublePoint linePt2)
    {
      const double tolerance = 1.0e-8;
      if (Math.Abs(linePt1.X - linePt2.X) < tolerance &&
        Math.Abs(linePt1.Y - linePt2.Y) < tolerance) return linePt1;
      double q = ((pt.X - linePt1.X) * (linePt2.X - linePt1.X) +
        (pt.Y - linePt1.Y) * (linePt2.Y - linePt1.Y)) /
        ((linePt2.X - linePt1.X) * (linePt2.X - linePt1.X) +
        (linePt2.Y - linePt1.Y) * (linePt2.Y - linePt1.Y));
      return new DoublePoint((1 - q) * linePt1.X + q * linePt2.X,
        (1 - q) * linePt1.Y + q * linePt2.Y);
    }
    //------------------------------------------------------------------------------

    private static double Distance(DoublePoint pt1, DoublePoint pt2)
    {
      double dx = pt2.X - pt1.X;
      double dy = pt2.Y - pt1.Y;
      return Math.Sqrt(dx * dx + dy * dy);
    }
    //------------------------------------------------------------------------------

    private static DoublePoint Rotate90(DoublePoint pt, DoublePoint center)
    {
      double dx = pt.X - center.X;
      double dy = pt.Y - center.Y;
      return new DoublePoint(center.X - dy, center.Y + dx);
    }
    //------------------------------------------------------------------------------

    private static DoublePoint PointAtAngle(DoublePoint origin, double distance, double angleRadians)
    {
      double sinA = Math.Sin(angleRadians);
      double cosA = Math.Cos(angleRadians);
      return new DoublePoint(distance * cosA + origin.X, -distance * sinA + origin.Y);
    }
    //------------------------------------------------------------------------------

    private bool GetEllipseFrom3Points(IntPoint pt1, IntPoint pt2, IntPoint pt3,
      out DoublePoint center, out double radius1, out double radius2, out double angle)
    {
      const double tolerance = 1.0e-8;
      //premise: pt1 & pt3 define the extents of one axis and pt2 is another point on the ellipse.
      DoublePoint dp1 = new DoublePoint(pt1);
      DoublePoint dp2 = new DoublePoint(pt2);
      DoublePoint dp3 = new DoublePoint(pt3);
      center = new DoublePoint((dp1.X + dp3.X) / 2, (dp1.Y + dp3.Y) / 2);

      double dx = dp1.X - center.X;
      double dy = dp1.Y - center.Y;
      //get the angle of the radius1 axis ...
      if (Math.Abs(dp1.X - dp3.X) == 0)
        angle = (dy > 0 ? 270 : 90);
      else
        angle = Math.Atan2(-dy, dx) * 180 / Math.PI;

      radius1 = Distance(dp1, center);
      radius2 = 0;
      DoublePoint cpol = ClosestPointOnLine(dp2, dp1, center);
      double rad1 = Distance(cpol, center);
      if (rad1 > radius1) return false;
      DoublePoint minorPt = Rotate90(dp1, center);
      cpol = ClosestPointOnLine(dp2, minorPt, center);
      double rad2 = Distance(cpol, center);
      double anglePt2 = Math.Acos(rad1 / radius1);
      double asin = Math.Sin(anglePt2);
      if (asin < tolerance) return false;
      radius2 = rad2 / asin;
      return (radius1 > tolerance && radius2 > tolerance);
    }
    //------------------------------------------------------------------------------

    internal static void Rotate(Path pts, DoublePoint centerPt, double angle)
    {
      double asin, acos;
      sincos(angle * rad180/180, out asin, out acos);
      for (int i = 0; i < pts.Count; i++)
      {
        double x = (double)pts[i].X - centerPt.X;
        double y = (double)pts[i].Y - centerPt.Y;
        pts[i] = new IntPoint(
          (x * acos) + (y * asin) + centerPt.X,
          (y * acos) - (x * asin) + centerPt.Y);
      }
    }
    //------------------------------------------------------------------------------

    internal override Path Reconstruct(IntPoint pt1, IntPoint pt2)
    {
      //a partial ellipse can't be defined by just 3 points ...
      return new Path();
    }
    //------------------------------------------------------------------------------

  }; //end EllipseCurve
  //------------------------------------------------------------------------------


  internal class OpenCurve : Curve
  {

    internal OpenCurve(Path ctrlPts, UInt16 refID, double precision) :
      base(ctrlPts, refID, precision) { } //constructor

    internal override CurveType GetCurveType() { return CurveType.Open; }

    internal override void SetCtrlPoints(Path ctrlPts, UInt16 refID, double precision)
    {
      int highpts = ctrlPts.Count - 1;
      if (highpts < 2) throw new CurveException("Open path requires 2 or more control points.");
      base.SetCtrlPoints(ctrlPts, refID, precision);
    }
    //------------------------------------------------------------------------------

    internal override Path FlattenedPath()
    {
      return new Path();
    }
    //------------------------------------------------------------------------------

    internal override Path Reconstruct(IntPoint pt1, IntPoint pt2)
    {
      return new Path();
    }
    //------------------------------------------------------------------------------
  }; //end OpenCurve
  //------------------------------------------------------------------------------

  internal class ClosedCurve : Curve
  {

    internal ClosedCurve(Path ctrlPts, UInt16 refID, double precision) :
      base(ctrlPts, refID, precision) { } //constructor

    internal override CurveType GetCurveType() { return CurveType.Closed; }

    internal override void SetCtrlPoints(Path ctrlPts, UInt16 refID, double precision)
    {
      int highpts = ctrlPts.Count - 1;
      if (highpts < 2) throw new CurveException("Closed path requires 3 or more control points.");
      base.SetCtrlPoints(ctrlPts, refID, precision);
    }
    //------------------------------------------------------------------------------

    internal override Path FlattenedPath()
    {
      return new Path();
    }
    //------------------------------------------------------------------------------

    internal override Path Reconstruct(IntPoint pt1, IntPoint pt2)
    {
      return new Path();
    }
    //------------------------------------------------------------------------------
  }; //end ClosedCurve
  //------------------------------------------------------------------------------

  class CurveException : Exception
  {
      public CurveException(string description) : base(description){}
  }
}

/*******************************************************************************
*                                                                              *
* Author    :  Angus Johnson                                                   *
* Version   :  1.0                                                             *
* Date      :  27 August 2013                                                  *
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

  public enum CurveType { CubicBezier, QuadBezier, Arc, Other };

  public class CurveList
  {
    private const double DefaultPrecision = 0.5;
    private List<Curve> m_Curves = new List<Curve>();

    public CurveList(double precision = DefaultPrecision)
    {
      Precision = (precision <= 0 ? DefaultPrecision : precision);  
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
      if (c is CBezierCurve) return CurveType.CubicBezier;
      else if (c is QBezierCurve) return CurveType.QuadBezier;
      else if (c is ArcCurve) return CurveType.Arc;
      else return CurveType.Other;
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

    public Paths GetFlattenedPaths()
    {
      Paths result = new Paths(m_Curves.Count);
      if (m_Curves.Count > 0) 
        foreach (Curve c in m_Curves)
          result.Add(c.FlattenedPath());
      return result;
    }
    //------------------------------------------------------------------------------

    public static Path Flatten(Path ctrlPts, CurveType ct, double precision = DefaultPrecision)
    {
      Curve curve = null;
      switch (ct)
      {
        case CurveType.QuadBezier:
          if (ctrlPts.Count > 2) curve = new QBezierCurve(ctrlPts, 0, precision); 
          break;
        case CurveType.CubicBezier:
          if (ctrlPts.Count > 3) curve = new CBezierCurve(ctrlPts, 0, precision);
          break;
        case CurveType.Arc:
          if (ctrlPts.Count > 2) curve = new ArcCurve(ctrlPts, 0, precision);
          break;
      }
      if (curve != null)
        return curve.FlattenedPath();
      else
        return new Path();
    }
    //------------------------------------------------------------------------------

    public static Paths Flatten(Paths paths, CurveType ct, double precision = DefaultPrecision)
    {
      int len = paths.Count;
      Paths result = new Paths(len);
      if (len == 0) return result;
      foreach (Path p in paths)
      {
        Path pp = Flatten(p, ct, precision);
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
      CurveType beztype;
      Curve.UnMakeZ(Pt1.Z, out beztype, out seg, out refId); //nb: just need refId
      if (refId >= 0 && refId < m_Curves.Count)
        result = m_Curves[refId].Reconstruct(Pt1, Pt2);
      return result;       
    }
    //------------------------------------------------------------------------------

  }; //end CurveList
  //------------------------------------------------------------------------------
  //------------------------------------------------------------------------------

  internal class Curve
  {
    internal const double half = 0.5;
    internal UInt16 reference;
    internal Path path = new Path(); //path of Control Pts
    //segments: ie supports poly-beziers (ie before flattening) with up to 4,096 segments 
    //internal List<Segment> segments = new List<Segment>();

    internal static Int64 MakeZ(CurveType ct, UInt16 seg, UInt16 refId, UInt32 idx)
    {
      //nb: SOP flag (bit63) is set separately
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

    internal static Int64 Round(double value)
    {
      return value < 0 ? (Int64)(value - 0.5) : (Int64)(value + 0.5);
    }
    //------------------------------------------------------------------------------

    ~Curve() { Clear(); }
    //------------------------------------------------------------------------------

    internal virtual void Clear() { path.Clear(); }
    //------------------------------------------------------------------------------

    internal Curve(Path ctrlPts, UInt16 refID, double precision)
    {
      SetCtrlPoints(ctrlPts, refID, precision);
    }
    //------------------------------------------------------------------------------

    internal virtual void SetCtrlPoints(Path ctrlPts, UInt16 refID, double precision)
    {
      Clear();
      reference = refID;
      path = new Path(ctrlPts);
    }
    //------------------------------------------------------------------------------

    internal virtual Path FlattenedPath()
    {
      int Len = path.Count;
      Path result = new Path(Len);
      for (int i = 0; i < Len; i++)
      {
        result.Add(new IntPoint(
          path[i].X, 
          path[i].Y, 
          MakeZ(CurveType.Other, 0, reference, (uint)i)));
      }
      return result;
    }

    //------------------------------------------------------------------------------

    internal virtual Path Reconstruct(IntPoint startZ, IntPoint endZ)
    {
      return new Path();
    }
    //------------------------------------------------------------------------------

  }; //end Curve
  //------------------------------------------------------------------------------

  internal class BezierCurve : Curve
  {
    //segments: ie supports poly-beziers (ie before flattening) with up to 16,383 segments 
    internal List<Segment> segments = new List<Segment>();
    internal CurveType beziertype;

    internal class IntNode
    {
      internal int val;
      internal IntNode next;
      internal IntNode prev;
      internal IntNode(int val) { this.val = val; }
    }

    internal IntNode InsertInt(IntNode insertAfter, int val)
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

    internal IntNode GetFirstIntNode(IntNode current)
    {
      if (current == null) return null;
      IntNode result = current;
      while (result.prev != null)
        result = result.prev;
      //now skip the very first (dummy) node ...
      return result.next;
    }
    //------------------------------------------------------------------------------

    internal DoublePoint MidPoint(IntPoint ip1, IntPoint ip2)
    {
      return new DoublePoint((double)(ip1.X + ip2.X) / 2, (double)(ip1.Y + ip2.Y) / 2);
    }
    //------------------------------------------------------------------------------

    internal UInt32 GetMostSignificantBit(UInt32 v) //index is zero based
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

    internal bool IsBitSet(UInt32 val, Int32 index)
    {
      return (val & (1 << (int)index)) != 0;
    }
    //------------------------------------------------------------------------------

    internal bool Odd(Int32 val)
    {
      return (val % 2) != 0;
    }
    //------------------------------------------------------------------------------

    internal bool Even(Int32 val)
    {
      return (val % 2) == 0;
    }
    //------------------------------------------------------------------------------

    internal class Segment
    {
      internal CurveType beziertype;
      internal UInt16 RefID;
      internal UInt16 SegID;
      internal UInt32 Index;
      internal DoublePoint[] Ctrls = new DoublePoint[4];
      internal Segment[] Childs = new Segment[2];
      internal Segment(UInt16 refID, UInt16 segID, UInt32 idx)
      {
        this.RefID = refID;
        this.SegID = segID;
        this.Index = idx;
      }

      internal void GetFlattenedSeg(Path path, bool init)
      {
        if (init)
        {
          Int64 Z = MakeZ(beziertype, SegID, RefID, Index);
          path.Add(new IntPoint(Round(Ctrls[0].X), Round(Ctrls[0].Y), Z));
        }

        if (Childs[0] == null)
        {
          int CtrlIdx = (beziertype == CurveType.CubicBezier ? 3 : 2);
          Int64 Z = MakeZ(beziertype, SegID, RefID, Index);
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
        switch (beziertype)
        {
          case CurveType.CubicBezier:
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
        UInt16 refID, UInt16 segID, UInt32 idx, double precision)
        : base(refID, segID, idx)
      {

        beziertype = CurveType.CubicBezier;
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
        Childs[0] = new CBezSegment(pt1, p12, p123, p1234, refID, segID, idx, precision);
        Childs[1] = new CBezSegment(p1234, p234, p34, pt4, refID, segID, idx + 1, precision);
      } //end CubicBez constructor
    } //end CubicBez

    internal class QBezSegment : Segment
    {
      internal QBezSegment(DoublePoint pt1, DoublePoint pt2, DoublePoint pt3,
        UInt16 refID, UInt16 segID, UInt32 idx, double precision)
        : base(refID, segID, idx)
      {

        beziertype = CurveType.QuadBezier;
        Ctrls[0] = pt1; Ctrls[1] = pt2; Ctrls[2] = pt3;
        //assess curve flatness:
        if (Math.Abs(pt1.X + pt3.X - 2 * pt2.X) + Math.Abs(pt1.Y + pt3.Y - 2 * pt2.Y) < precision) return;

        //if not at maximum precision then (recursively) create sub-segments ...
        //DoublePoint p12, p23, p123;
        DoublePoint p12 = new DoublePoint((pt1.X + pt2.X) * half, (pt1.Y + pt2.Y) * half);
        DoublePoint p23 = new DoublePoint((pt2.X + pt3.X) * half, (pt2.Y + pt3.Y) * half);
        DoublePoint p123 = new DoublePoint((p12.X + p23.X) * half, (p12.Y + p23.Y) * half);
        idx = idx << 1;
        Childs[0] = new QBezSegment(pt1, p12, p123, refID, segID, idx, precision);
        Childs[1] = new QBezSegment(p123, p23, pt3, refID, segID, idx + 1, precision);
      } //end QuadBez constructor
    } //end QuadBez
    //------------------------------------------------------------------------------

    internal BezierCurve(Path ctrlPts, UInt16 refID, double precision) :
      base(ctrlPts, refID, precision) { }
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

      bool reversed = false;
      if (pt2.Z < 0)
      {
        IntPoint tmp = pt1;
        pt1 = pt2;
        pt2 = tmp;
        reversed = true;
      }

      CurveType bt1, bt2;
      UInt16 seg1, seg2;
      UInt16 ref1, ref2;
      UInt32 pt1Z = UnMakeZ(pt1.Z, out bt1, out seg1, out ref1);
      UInt32 pt2Z = UnMakeZ(pt2.Z, out bt2, out seg2, out ref2);

      if (bt1 != beziertype || bt1 != bt2 ||
        ref1 != reference || ref1 != ref2) return out_poly;

      if (seg1 >= segments.Count || seg2 >= segments.Count) return out_poly;

      if (seg1 > seg2)
      {
        UInt16 i = seg1;
        seg1 = seg2;
        seg2 = i;
        UInt32 tmp = pt1Z;
        pt1Z = pt2Z;
        pt2Z = tmp;
      }

      //do further checks for reversal, in case reversal within a single segment ...
      if (!reversed && seg1 == seg2 && pt1Z != 1 && pt2Z != 1)
      {
        UInt32 i = GetMostSignificantBit(pt1Z);
        UInt32 j = GetMostSignificantBit(pt2Z);
        UInt32 k = Math.Max(i, j);
        //nb: we must compare Node indexes at the same level ...
        i = pt1Z << (int)(k - i);
        j = pt2Z << (int)(k - j);
        if (i > j)
        {
          UInt32 tmp = pt1Z;
          pt1Z = pt2Z;
          pt2Z = tmp;
          reversed = true;
        }
      }

      while (seg1 <= seg2)
      {
        //create a dummy first IntNode for the Int List ...
        IntNode intList = new IntNode(0);
        IntNode intCurr = intList;

        if (seg1 != seg2)
          ReconstructInternal(seg1, pt1Z, 1, intCurr);
        else
          ReconstructInternal(seg1, pt1Z, pt2Z, intCurr);

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
      if (reversed) out_poly.Reverse();
      return out_poly;
    }
    //------------------------------------------------------------------------------

    internal void ReconstructInternal(UInt16 segIdx, UInt32 startIdx, UInt32 endIdx, IntNode intCurr)
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
  //------------------------------------------------------------------------------

  internal class CBezierCurve : BezierCurve
  {

    internal CBezierCurve(Path ctrlPts, UInt16 refID, double precision) :
      base(ctrlPts, refID, precision) { beziertype = CurveType.CubicBezier; }
    //------------------------------------------------------------------------------

    internal override void SetCtrlPoints(Path ctrlPts, UInt16 refID, double precision)
    {
      int highpts = ctrlPts.Count - 1;
      if (highpts < 3)  throw new CurveException("CubicBezier: insuffient control points.");
      else highpts -= highpts % 3;
      base.SetCtrlPoints(ctrlPts, refID, precision);
      for (UInt16 i = 0; i < ((UInt16)highpts / 3); ++i)
      {
        CBezSegment s = new CBezSegment(
                    new DoublePoint(ctrlPts[i * 3]),
                    new DoublePoint(ctrlPts[i * 3 + 1]),
                    new DoublePoint(ctrlPts[i * 3 + 2]),
                    new DoublePoint(ctrlPts[i * 3 + 3]),
                    refID, i, 1, precision);
        segments.Add(s);
      }
    }
    //------------------------------------------------------------------------------

  }; //end CBezierCurve
  //------------------------------------------------------------------------------
  //------------------------------------------------------------------------------

  internal class QBezierCurve : BezierCurve
  {

    internal QBezierCurve(Path ctrlPts, UInt16 refID, double precision) :
      base(ctrlPts, refID, precision) { beziertype = CurveType.QuadBezier; }

    internal override void SetCtrlPoints(Path ctrlPts, UInt16 refID, double precision)
    {
      int highpts = ctrlPts.Count - 1;
      if (highpts < 2) throw new CurveException("QuadBezier: insuffient control points.");
      else highpts -= highpts % 2;
      base.SetCtrlPoints(ctrlPts, refID, precision);
      for (UInt16 i = 0; i < ((UInt16)highpts / 2); ++i)
      {
        QBezSegment s = new QBezSegment(
                          new DoublePoint(ctrlPts[i * 2]),
                          new DoublePoint(ctrlPts[i * 2 + 1]),
                          new DoublePoint(ctrlPts[i * 2 + 2]),
                          refID, i, 1, precision);
        segments.Add(s);
      }
    }
    //------------------------------------------------------------------------------

  }; //end QBezierCurve
  //------------------------------------------------------------------------------
  //------------------------------------------------------------------------------

  internal class ArcCurve : Curve
  {
    double m_precision;

    internal double GetAngleOfEndPtFromStartPt(DoublePoint StartPt, DoublePoint EndPt)
    {
      const double rad90 = Math.PI / 2;
      const double rad270 = rad90 * 3;
      const double rad360 = Math.PI * 2;

      double result;
      DoublePoint relEndPt = new DoublePoint(EndPt.X - StartPt.X, EndPt.Y - StartPt.Y);
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

    const double TwoPI = Math.PI * 2;

    internal double GetBisectingAngle(double angle1, double angle2, bool isClockwise)
    {
      if (isClockwise && angle2 >= angle1) angle2 -= TwoPI;
      else if (!isClockwise && angle2 <= angle1) angle2 += TwoPI;
      double result = (angle1 + angle2) / 2;
      if (result < 0) result += TwoPI;
      if (result > TwoPI) result -= TwoPI;
      return result;
    }
    //------------------------------------------------------------------------------

    internal void sincos(double angle, out double sinval, out double cosval)
    {
      if (angle < 0) angle += (2 * Math.PI);
      sinval = Math.Sin(angle);
      cosval = Math.Sqrt(1.0 - sinval * sinval);
      if (angle > Math.PI / 2 && angle < Math.PI * 3 / 2) cosval = -cosval;
    }
    //------------------------------------------------------------------------------

    IntPoint GetPointFromOrigin(DoublePoint origin, double radius, double angle)
    {
      double asin, acos;
      sincos(angle, out asin, out acos);
      return new IntPoint(
        Round(radius * acos + origin.X), 
        Round(-radius * asin + origin.Y));
    }
    //------------------------------------------------------------------------------

    internal bool CircleFrom3Points(DoublePoint p1, DoublePoint p2, DoublePoint p3, 
      out DoublePoint origin, out double radius)
    {
      origin = new DoublePoint();
      radius = 0.0;
      double m1, m2, mp1x, mp1y, mp2x, mp2y;
      if (p1.Y == p2.Y) 
      {
        //test for collinear points ...
        if ((p3.Y-p1.Y)*(p2.X-p3.X) == (p3.X-p1.X)*(p3.Y-p2.Y)) return false;
        m1 = (p3.X-p1.X) / (p1.Y-p3.Y); //nb: inverse slopes
        m2 = (p2.X - p3.X) / (p3.Y - p2.Y);
        mp1x = (p1.X + p3.X) / 2;
        mp1y = (p1.Y + p3.Y) / 2;
        mp2x = (p3.X + p2.X) / 2;
        mp2y = (p3.Y + p2.Y) / 2;
      }
      else if (p2.Y == p3.Y) 
      {
        if ((p1.Y-p2.Y)*(p1.X-p3.X) == (p2.X-p1.X)*(p3.Y-p1.Y)) return false;
        m1 = (p2.X - p1.X) / (p1.Y - p2.Y); //nb: inverse slopes
        m2 = (p1.X - p3.X) / (p3.Y - p1.Y);
        mp1x = (p1.X + p2.X) / 2;
        mp1y = (p1.Y + p2.Y) / 2;
        mp2x = (p3.X + p1.X) / 2;
        mp2y = (p3.Y + p1.Y) / 2;
      }
      else //use 1-2, 2-3
      {
        if ((p1.Y-p2.Y)*(p2.X-p3.X) == (p2.X-p1.X)*(p3.Y-p2.Y)) return false;
        m1 = (p2.X - p1.X) / (p1.Y - p2.Y); //nb: inverse slopes
        m2 = (p2.X - p3.X) / (p3.Y - p2.Y);
        mp1x = (p1.X + p2.X) / 2;
        mp1y = (p1.Y + p2.Y) / 2;
        mp2x = (p3.X + p2.X) / 2;
        mp2y = (p3.Y + p2.Y) / 2;
      }
      double b1 = mp1y - mp1x * m1 ;
      double b2 = mp2y - mp2x * m2;
      origin.X = (b2 - b1) / (m1 - m2);
      origin.Y = m1 * origin.X + b1;
      radius = Math.Sqrt((p1.X - origin.X) * (p1.X - origin.X) +
        (p1.Y - origin.Y) * (p1.Y - origin.Y));
      return true;
    }
    //---------------------------------------------------------------------------

    internal ArcCurve(Path ctrlPts, UInt16 refID, double precision) :
      base(ctrlPts, refID, precision) { } //constructor

    internal override void SetCtrlPoints(Path ctrlPts, UInt16 refID, double precision)
    {
      int highpts = ctrlPts.Count - 1;
      if (highpts < 2) throw new CurveException("Arc: insuffient control points.");
      else highpts -= highpts % 2;
      base.SetCtrlPoints(ctrlPts, refID, precision);
      m_precision = precision;
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
        bool isClockwise;

        if (!CircleFrom3Points(p1, p2, p3, out origin, out radius))
          return result; //oops
        double a1 = GetAngleOfEndPtFromStartPt(p1, origin);
        double a2 = GetAngleOfEndPtFromStartPt(p2, origin);
        double a3 = GetAngleOfEndPtFromStartPt(p3, origin);
        isClockwise = (((a2 > a1) == (a2 < a3)) != (a1 < a3));

        double frac = Math.Abs(a3 - a1) / (2 * Math.PI);
        if (isClockwise == (a3 >= a1)) frac = 1 - frac;

        int steps = (int)Round(Math.PI / Math.Acos(1 - m_precision / radius)) + 1;
        if (steps < 2) steps = 2;
        double asin, acos, angle = frac * 2 * Math.PI / steps;
        if (!isClockwise) angle = -angle;
        sincos(angle, out asin, out acos);
        double x = ((double)p1.X - origin.X);
        double y = ((double)p1.Y - origin.Y);
        for (int j = 0; j < steps; j++)
        {
          result.Add(new IntPoint(
            Round(origin.X + x),
            Round(origin.Y + y),
            MakeZ(CurveType.Arc, (UInt16)((i - 1) / 2), reference, (UInt16)j)));
          double x2 = x;
          x = x * acos - asin * y;
          y = x2 * asin + y * acos;
        }
        p1 = p3;
      }
      return result;
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
      
      if (seg1 > seg2 || (seg1 == seg2 && z1 > z2))
      {
        UInt16 i = seg1;
        seg1 = seg2;
        seg2 = i;
        IntPoint tmp = pt1;
        pt1 = pt2;
        pt2 = tmp;
      }

      int idx1 = seg1 * 2, idx2 = seg2 * 2 + 2;
      result.Capacity = idx2 - idx1 + 1;
      DoublePoint origin1 = new DoublePoint();
      double radius1;

      for (int j = seg1; j <= seg2; j++)
      {
        if ((j == seg1 && pt1 != path[j * 2]) || (j == seg2 && pt2 != path[j * 2 + 2]))
        {
          DoublePoint dp1 = new DoublePoint(path[j * 2]);
          DoublePoint dp2 = new DoublePoint(path[j * 2 + 1]);
          DoublePoint dp3 = new DoublePoint(path[j * 2 + 2]);
          //get the origin and radius of each arc ...
          CircleFrom3Points(dp1, dp2, dp3, out origin1, out radius1);
          double a1 = GetAngleOfEndPtFromStartPt(origin1, dp1);
          double a2 = GetAngleOfEndPtFromStartPt(origin1, dp2);
          double a3 = GetAngleOfEndPtFromStartPt(origin1, dp3);
          bool isClockwise = (((a2 > a1) == (a2 < a3)) != (a1 < a3));
          //now build the new arc
          if (j == seg1)
            a1 = GetAngleOfEndPtFromStartPt(origin1, new DoublePoint(pt1));
          if (j == seg2)
            a3 = GetAngleOfEndPtFromStartPt(origin1, new DoublePoint(pt2));
          a2 = GetBisectingAngle(a1, a3, isClockwise);
          result.Add(GetPointFromOrigin(origin1, radius1, a1));
          result.Add(GetPointFromOrigin(origin1, radius1, a2));
          if (j == seg2) result.Add(pt2);
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
  //------------------------------------------------------------------------------


  class CurveException : Exception
  {
      public CurveException(string description) : base(description){}
  }
}

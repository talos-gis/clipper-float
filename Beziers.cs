/*******************************************************************************
*                                                                              *
* Author    :  Angus Johnson                                                   *
* Version   :  0.8e (alpha)                                                    *
* Date      :  19 June 2013                                                    *
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

namespace BezierLib
{

  using Polygon = List<IntPoint>;
  using Polygons = List<List<IntPoint>>;

  public enum BezierType { CubicBezier, QuadBezier };

  struct DoublePoint{
    internal double x;
    internal double y;
    public DoublePoint(IntPoint ip){x = (double)ip.X; y = (double)ip.Y;}
    public DoublePoint(double x = 0, double y = 0){this.x = x; this.y = y;}
  }

  public class Bezier
  {
    internal const double half = 0.5;
    private int reference;
    internal BezierType beziertype;
    //segments: ie supports poly-beziers (ie before flattening) with up to 16,383 segments 
    internal List<Segment> segments = new List<Segment>();

    internal static Int64 MakeZ(BezierType beziertype, UInt16 segID, UInt16 refID, UInt32 idx)
    {
      Int32 hi = (Int32)beziertype << 30 | segID << 16 | refID;
      return (Int64)hi << 32 | idx;
    }
    //------------------------------------------------------------------------------

    internal static UInt32 UnMakeZ(Int64 zval, out BezierType beziertype, out UInt16 segID, out UInt16 refID)
    {
      Int64 vals = zval >> 32; //the top 32 bits => vals
      beziertype = (BezierType)(vals >> 30);
      segID = (UInt16)(((vals >> 16) & 0x3FFF) - 1); //convert segments to zero-base
      refID = (UInt16)(vals & 0xFFFF);
      return (UInt32)(zval & 0xFFFFFFFF);
    }
    //------------------------------------------------------------------------------

    internal class IntNode
    {
      internal int val;
      internal IntNode next;
      internal IntNode prev;
      internal IntNode(int val) { this.val = val;}
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
      UInt32[] b = {0x2, 0xC, 0xF0, 0xFF00, 0xFFFF0000};
      Int32[] s = {0x1, 0x2, 0x4, 0x8, 0x10};
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

    internal static Int64 Round(double value)
    {
      return value < 0 ? (Int64)(value - 0.5) : (Int64)(value + 0.5);
    }

    //------------------------------------------------------------------------------
    //------------------------------------------------------------------------------

    internal class Segment
    {
      internal BezierType beziertype;
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

      internal void GetFlattenedPath(Polygon path, bool init)
      {
        if (init)
        {
          Int64 Z = MakeZ(beziertype, SegID, RefID, Index);
          path.Add(new IntPoint(Round(Ctrls[0].x), Round(Ctrls[0].y), Z));
        }

        if (Childs[0] == null)
        {
          int CtrlIdx = (beziertype == BezierType.CubicBezier ? 3: 2);
          Int64 Z = MakeZ(beziertype, SegID, RefID, Index);
          path.Add(new IntPoint(Round(Ctrls[CtrlIdx].x), Round(Ctrls[CtrlIdx].y), Z));
        }
        else
        {
          Childs[0].GetFlattenedPath(path, false);
          Childs[1].GetFlattenedPath(path, false);
        }
      }

    } //end Segment
    //------------------------------------------------------------------------------

    private class CubicBez : Segment
    {
      internal CubicBez(DoublePoint pt1, DoublePoint pt2, DoublePoint pt3, DoublePoint pt4,
        UInt16 refID, UInt16 segID, UInt32 idx, double precision): base(refID, segID, idx)
      {

        beziertype = BezierType.CubicBezier;
        Ctrls[0] = pt1; Ctrls[1] = pt2; Ctrls[2] = pt3; Ctrls[3] = pt4;
        //assess curve flatness:
        //http://groups.google.com/group/comp.graphics.algorithms/tree/browse_frm/thread/d85ca902fdbd746e
        if (Math.Abs(pt1.x + pt3.x - 2*pt2.x) + Math.Abs(pt2.x + pt4.x - 2*pt3.x) +
          Math.Abs(pt1.y + pt3.y - 2*pt2.y) + Math.Abs(pt2.y + pt4.y - 2*pt3.y) < precision)
            return;

        //if not at maximum precision then (recursively) create sub-segments ...
        DoublePoint p12, p23, p34, p123, p234, p1234;
        p12.x = (pt1.x + pt2.x) * half;
        p12.y = (pt1.y + pt2.y) * half;
        p23.x = (pt2.x + pt3.x) * half;
        p23.y = (pt2.y + pt3.y) * half;
        p34.x = (pt3.x + pt4.x) * half;
        p34.y = (pt3.y + pt4.y) * half;
        p123.x = (p12.x + p23.x) * half;
        p123.y = (p12.y + p23.y) * half;
        p234.x = (p23.x + p34.x) * half;
        p234.y = (p23.y + p34.y) * half;
        p1234.x = (p123.x + p234.x) * half;
        p1234.y = (p123.y + p234.y) * half;
        idx = idx << 1;
        Childs[0] = new CubicBez(pt1, p12, p123, p1234, refID, segID, idx, precision);
        Childs[1] = new CubicBez(p1234, p234, p34, pt4, refID, segID, idx +1, precision);
      } //end CubicBez constructor
    } //end CubicBez

    private class QuadBez : Segment
    {
      internal QuadBez(DoublePoint pt1, DoublePoint pt2, DoublePoint pt3,
        UInt16 refID, UInt16 segID, UInt32 idx, double precision): base(refID, segID, idx)
      {

        beziertype = BezierType.QuadBezier;
        Ctrls[0] = pt1; Ctrls[1] = pt2; Ctrls[2] = pt3;
        //assess curve flatness:
        if (Math.Abs(pt1.x + pt3.x - 2*pt2.x) + Math.Abs(pt1.y + pt3.y - 2*pt2.y) < precision) return;

        //if not at maximum precision then (recursively) create sub-segments ...
        DoublePoint p12, p23, p123;
        p12.x = (pt1.x + pt2.x) * half;
        p12.y = (pt1.y + pt2.y) * half;
        p23.x = (pt2.x + pt3.x) * half;
        p23.y = (pt2.y + pt3.y) * half;
        p123.x = (p12.x + p23.x) * half;
        p123.y = (p12.y + p23.y) * half;
        idx = idx << 1;
        Childs[0] = new QuadBez(pt1, p12, p123, refID, segID, idx, precision);
        Childs[1] = new QuadBez(p123, p23, pt3, refID, segID, idx +1, precision);
      } //end QuadBez constructor
    } //end QuadBez
    //------------------------------------------------------------------------------

    public Bezier(){}
    //------------------------------------------------------------------------------

    public Bezier(Polygon ctrlPts, BezierType beztype, UInt16 refID, double precision = 0.5)
    {
      SetCtrlPoints(ctrlPts, beztype, refID, precision);
    }
    //------------------------------------------------------------------------------

    public void SetCtrlPoints(Polygon ctrlPts, BezierType beztype, UInt16 refID, double precision = 0.5)
    {
      //clean up any existing data ...
      segments.Clear();

      this.beziertype = beztype;
      this.reference = refID;

      int highpts = ctrlPts.Count - 1;

      switch( beztype )
      {
        case BezierType.CubicBezier:
          if (highpts < 3)  throw new BezierException("CubicBezier: insuffient control points.");
          else highpts -= highpts % 3;
          break;
        case BezierType.QuadBezier:
          if (highpts < 2) throw new BezierException("QuadBezier: insuffient control points.");
          else highpts -= highpts % 2;
          break;
        default: throw new BezierException("Unsupported bezier type");
      }

      if (precision <= 0.0) precision = 0.1;

      //now for each segment in the poly-bezier create a binary tree structure
      //and add it to SegmentList ...
      switch( beztype )
      {
        case BezierType.CubicBezier:
          for (int i = 0; i < ((int)highpts / 3); ++i)
          {
            Segment s = new CubicBez(
                        new DoublePoint(ctrlPts[i*3]),
                        new DoublePoint(ctrlPts[i*3+1]),
                        new DoublePoint(ctrlPts[i*3+2]),
                        new DoublePoint(ctrlPts[i*3+3]),
                        refID, (UInt16)(i+1), 1, precision);
            segments.Add(s);
          }
          break;
        case BezierType.QuadBezier:
          for (int i = 0; i < ((int)highpts / 2); ++i)
          {
            Segment s = new QuadBez(
                        new DoublePoint(ctrlPts[i*2]),
                        new DoublePoint(ctrlPts[i*2+1]),
                        new DoublePoint(ctrlPts[i*2+2]),
                        refID, (UInt16)(i+1), 1, precision);
            segments.Add(s);
          }
          break;
      }
    }
    //------------------------------------------------------------------------------

    public Polygon FlattenedPath()
    {
      Polygon path = new Polygon();
      for (int i = 0; i < segments.Count; i++)
        segments[i].GetFlattenedPath(path, i == 0);
      return path;
    }

  //------------------------------------------------------------------------------

  internal void AddCtrlPoint(Segment segment, Polygon ctrlPts)
  {
    int firstDelta = (ctrlPts.Count == 0 ? 0 : 1);
    switch (segment.beziertype)
    {
      case BezierType.CubicBezier:
        for (int i = firstDelta; i < 4; i++)
          ctrlPts.Add(new IntPoint(Round(segment.Ctrls[i].x), Round(segment.Ctrls[i].y)));
        break;
      case BezierType.QuadBezier:
        for (int i = firstDelta; i < 3; i++)
          ctrlPts.Add(new IntPoint(Round(segment.Ctrls[i].x), Round(segment.Ctrls[i].y)));
        break;
    }
  }
  //------------------------------------------------------------------------------

  public Polygon Reconstruct(Int64 startZ, Int64 endZ)
  {
    Polygon out_poly = new Polygon();
    BezierType bt1, bt2;
    UInt16 seg1, seg2;
    UInt16 ref1, ref2;
    UInt32 startZu = UnMakeZ(startZ, out bt1, out seg1, out ref1);
    UInt32 endZu = UnMakeZ(endZ, out bt2, out seg2, out ref2);

    if (bt1 != beziertype || bt1 != bt2 ||
      ref1 != reference || ref1 != ref2) return out_poly;

    if (seg1 >= segments.Count || seg2 >= segments.Count) return out_poly;

    //check orientation because it's much simpler to temporarily unreverse when
    //the startIdx and endIdx are reversed ...
    bool reversed = (seg1 > seg2);
    if (reversed)
    {
      UInt16 i = seg1;
      seg1 = seg2;
      seg2 = i;
      UInt32 j = startZu;
      startZu = endZu;
      endZu = j;
    }

    //do further checks for reversal, in case reversal within a single segment ...
    if (!reversed && seg1 == seg2 && startZ != 1 && endZ != 1)
    {
      UInt32 i = GetMostSignificantBit(startZu);
      UInt32 j = GetMostSignificantBit(endZu);
      UInt32 k = Math.Max(i, j);
      //nb: we must compare Node indexes at the same level ...
      i = startZu << (int)(k - i);
      j = endZu << (int)(k - j);
      if (i > j)
      {
        k = startZu;
        startZu = endZu;
        endZu = k;
        reversed = true;
      }
    }

    while (seg1 <= seg2)
    {
      //create a dummy first IntNode for the Int List ...
      IntNode intList = new IntNode(0);
      IntNode intCurr = intList;

      if (seg1 != seg2)
        ReconstructInternal(seg1, startZu, 1, intCurr);
      else
        ReconstructInternal(seg1, startZu, endZu, intCurr);

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
        AddCtrlPoint(s, out_poly);
        intCurr = intCurr.next;
      } //while 

      intList = null;
      seg1++;
      startZu = 1;
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
      R = (1 << (int)((Level +1))) - 1;
    } else
    {
      int k = (int)(Level - L2);
      R = ((int)endIdx << k) + (1 << k) - 1;
    }

    if (startIdx == 1) //special case
    {
      //Left marker (L) is bottom left of the binary tree ...
      L = (1 << (int)Level);
      L1 = Level;
    } else
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
        while (Odd(R) && ((R-1) << j >= L)) 
        {
          R = R >> 1; //go up a level
          j++;
        }
        InsertInt(intCurr, R); //nb: doesn't update IntCurrent
        R--;
      } while (R != (3 << (int)(Level - j)) -1 && ((R << j) > L));
    
  }
  //------------------------------------------------------------------------------

  } //end Bezier
  //------------------------------------------------------------------------------

  class BezierException : Exception
  {
      public BezierException(string description) : base(description){}
  }
}

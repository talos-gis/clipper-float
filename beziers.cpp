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

#include <vector>
#include "clipper.hpp"
#include "beziers.hpp"

namespace BezierLib {

  struct DoublePoint{
    double x;
    double y;
    DoublePoint(double _x = 0, double _y = 0): x(_x), y(_y){};
    DoublePoint(const IntPoint ip): x((double)ip.X), y((double)ip.Y){};
  };

  struct IntNode{
    int val;
    IntNode* next;
    IntNode* prev;
    IntNode(int _val): val(_val), next(0), prev(0){};
  };

  const double half = 0.5;

  //------------------------------------------------------------------------------
  // Miscellaneous helper functions ...
  //------------------------------------------------------------------------------

  //nb. The format (high to low) of the 64bit Z value returned in the path ...
  //Typ  (2): either CubicBezier, QuadBezier
  //Seg (14): segment index since a bezier may consist of multiple segments
  //Ref (16): reference value passed to TBezier owner object
  //Idx (32): binary index to sub-segment containing control points

  inline long64 MakeZ(BezierType beziertype, unsigned short seg, unsigned short ref, unsigned idx)
  {
    unsigned hi = beziertype << 30 | seg << 16 | ref;
    return (long64)hi << 32 | idx;
  };
  //------------------------------------------------------------------------------

  unsigned UnMakeZ(long64 zval, BezierType& beziertype, unsigned short& seg, unsigned short& ref)
  {
    unsigned vals = zval >> 32; //the top 32 bits => vals
    beziertype = BezierType(vals >> 30);
    seg = ((vals >> 16) & 0x3FFF) -1; //convert segments to zero-base
    ref = vals & 0xFFFF;
    return zval & 0xFFFFFFFF;
  };
  //------------------------------------------------------------------------------

  IntNode* InsertInt(IntNode* insertAfter, int val)
  {
    IntNode* result = new IntNode(val);
    result->next = insertAfter->next;
    result->prev = insertAfter;
    if (insertAfter->next)
      insertAfter->next->prev = result;
    insertAfter->next = result;
    return result;
  }
  //------------------------------------------------------------------------------

  IntNode* GetFirstIntNode(IntNode* current)
  {
    if (!current) return 0;
    IntNode* result = current;
    while (result->prev)
      result = result->prev;
    //now skip the very first (dummy) node ...
    return result->next;
  }
  //------------------------------------------------------------------------------

  void DisposeIntNodes(IntNode* intnodes)
  {
    if (!intnodes) return;
    while (intnodes->prev)
      intnodes = intnodes->prev;

    do {
      IntNode* intnode = intnodes;
      intnodes = intnodes->next;
      delete intnode;
    } while (intnodes);
  }
  //------------------------------------------------------------------------------

  DoublePoint MidPoint(const IntPoint& ip1, const IntPoint& ip2)
  {
    return DoublePoint(double(ip1.X + ip2.X) / 2, double(ip1.Y + ip2.Y) / 2);
  }
  //------------------------------------------------------------------------------

  unsigned GetMostSignificantBit(unsigned v) //index is zero based
  {
    const unsigned b[5] = {0x2, 0xC, 0xF0, 0xFF00, 0xFFFF0000};
    const unsigned s[5] = {0x1, 0x2, 0x4, 0x8, 0x10};
    unsigned result = 0;
    for (int i = 4; i >= 0; --i)
      if ((v & b[i]) != 0) 
      {
        v = v >> s[i];
        result = result | s[i];
      }
    return result;
  };
  //------------------------------------------------------------------------------

  inline bool IsBitSet(unsigned val, unsigned index)
  {
    return (val & (1 << index)) != 0;
  };
  //------------------------------------------------------------------------------

  inline bool Odd(const unsigned val)
  {
    return (val % 2) != 0;
  };
  //------------------------------------------------------------------------------

  inline bool Even(const unsigned val)
  {
    return (val % 2) == 0;
  };
  //------------------------------------------------------------------------------

  inline long64 Round(double val)
  {
    return (val < 0) ? static_cast<long64>(val - 0.5) : static_cast<long64>(val + 0.5);
  }

  //------------------------------------------------------------------------------
  // Segment class
  //------------------------------------------------------------------------------

  class Segment
  {
  public:
    BezierType beziertype;
    unsigned short RefID;
    unsigned short SegID;
    unsigned Index;
    DoublePoint Ctrls[4];
    Segment* Childs[2];
    Segment(unsigned short ref, unsigned short seg, unsigned idx): 
        RefID(ref), SegID(seg), Index(idx) {
          Childs[0] = 0;
          Childs[1] = 0;
    }; 
    ~Segment()
    {
      if (Childs[0]) delete Childs[0];
      if (Childs[1]) delete Childs[1];
    }
    void GetFlattenedPath(ClipperLib::Polygon& path, bool init)
    {
      if (init)
      {
        long64 Z = MakeZ(beziertype, SegID, RefID, Index);
        path.push_back(IntPoint(Round(Ctrls[0].x), Round(Ctrls[0].y), Z));
      } 
    
      if (!Childs[0])
      {
        int CtrlIdx = 3;
        if (beziertype == QuadBezier) CtrlIdx = 2;
        long64 Z = MakeZ(beziertype, SegID, RefID, Index);
        path.push_back(IntPoint(Round(Ctrls[CtrlIdx].x), Round(Ctrls[CtrlIdx].y), Z));
      } else
      {
        Childs[0]->GetFlattenedPath(path, false);
        Childs[1]->GetFlattenedPath(path, false);
      }
    }
  };
  //------------------------------------------------------------------------------

  void AddCtrlPoint(Segment* segment, ClipperLib::Polygon& ctrlPts)
  {
    int firstDelta = (ctrlPts.empty() ? 0 : 1);
    switch (segment->beziertype )
    {
      case CubicBezier:
        for (int i = firstDelta; i < 4; ++i)
        {
          ctrlPts.push_back(IntPoint(
            Round(segment->Ctrls[i].x), Round(segment->Ctrls[i].y)));
        }
        break;
      case QuadBezier:
        for (int i = firstDelta; i < 3; ++i)
        {
          ctrlPts.push_back(IntPoint(
            Round(segment->Ctrls[i].x), Round(segment->Ctrls[i].y)));
        }
        break;
    }
  }

  //------------------------------------------------------------------------------
  // CubicBez class
  //------------------------------------------------------------------------------

  class CubicBez: public Segment
  {
  public:
    CubicBez(const DoublePoint pt1, const DoublePoint pt2, 
      const DoublePoint pt3, const DoublePoint pt4,
      unsigned short ref, unsigned short seg, unsigned idx, double precision): Segment(ref, seg, idx)
    {
     
      beziertype = CubicBezier;
      Ctrls[0] = pt1; Ctrls[1] = pt2; Ctrls[2] = pt3; Ctrls[3] = pt4;
      //assess curve flatness:
      //http://groups.google.com/group/comp.graphics.algorithms/tree/browse_frm/thread/d85ca902fdbd746e
      if (abs(pt1.x + pt3.x - 2*pt2.x) + abs(pt2.x + pt4.x - 2*pt3.x) +
        abs(pt1.y + pt3.y - 2*pt2.y) + abs(pt2.y + pt4.y - 2*pt3.y) < precision)
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
      Childs[0] = new CubicBez(pt1, p12, p123, p1234, ref, seg, idx, precision);
      Childs[1] = new CubicBez(p1234, p234, p34, pt4, ref, seg, idx +1, precision);
    }
  };

  //------------------------------------------------------------------------------
  // QuadBez class
  //------------------------------------------------------------------------------

  class QuadBez: public Segment
  {
  public:
    QuadBez(const DoublePoint pt1, const DoublePoint pt2, const DoublePoint pt3,
      unsigned short ref, unsigned short seg, unsigned idx, double precision): Segment(ref, seg, idx)
    {
      beziertype = QuadBezier;
      Ctrls[0] = pt1; Ctrls[1] = pt2; Ctrls[2] = pt3;
      //assess curve flatness:
      if (std::abs(pt1.x + pt3.x - 2*pt2.x) + abs(pt1.y + pt3.y - 2*pt2.y) < precision)
        return;

      //if not at maximum precision then (recursively) create sub-segments ...
      DoublePoint p12, p23, p123;
      p12.x = (pt1.x + pt2.x) * half;
      p12.y = (pt1.y + pt2.y) * half;
      p23.x = (pt2.x + pt3.x) * half;
      p23.y = (pt2.y + pt3.y) * half;
      p123.x = (p12.x + p23.x) * half;
      p123.y = (p12.y + p23.y) * half;
      idx = idx << 1;
      Childs[0] = new QuadBez(pt1, p12, p123, ref, seg, idx, precision);
      Childs[1] = new QuadBez(p123, p23, pt3, ref, seg, idx +1, precision);
    }
  };

  //------------------------------------------------------------------------------
  // Bezier class
  //------------------------------------------------------------------------------

  Bezier::Bezier(
      const ClipperLib::Polygon& ctrlPts,  //CtrlPts: Bezier control points
      BezierType beztype,                  //CubicBezier or QuadBezier ...
      short ref,                           //Ref: user supplied identifier;
      double precision)                    //Precision of flattened path
  {
    SetCtrlPoints(ctrlPts, beztype, ref, precision);
  };
  //------------------------------------------------------------------------------

  void Bezier::SetCtrlPoints(const ClipperLib::Polygon& ctrlPts,
    BezierType beztype, unsigned short ref, double precision)
  {
    //clean up any existing data ...
    Clear();
    size_t highpts = ctrlPts.size() -1;

    this->beziertype = beztype;
    this->reference = ref;

    switch( beztype )
    {
      case CubicBezier:
        if (highpts < 3)  throw "CubicBezier: insuffient control points.";
        else highpts -= highpts % 3;
        break;
      case QuadBezier:
        if (highpts < 2)  throw "QuadBezier: insuffient control points.";
        else highpts -= highpts % 2;
        break;
      default: throw "Unsupported bezier type";
    }

    if (precision <= 0.0) precision = 0.1;

    //now for each segment in the poly-bezier create a binary tree structure
    //and add it to SegmentList ...
    switch( beztype )
    {
      case CubicBezier:
        for (int i = 0; i < ((int)highpts / 3); ++i)
        {
          Segment* s = new CubicBez(
                      DoublePoint(ctrlPts[i*3]),
                      DoublePoint(ctrlPts[i*3+1]),
                      DoublePoint(ctrlPts[i*3+2]),
                      DoublePoint(ctrlPts[i*3+3]),
                      ref, i+1, 1, precision);
          segments.push_back(s);
        }
        break;
      case QuadBezier:
        for (int i = 0; i < ((int)highpts / 2); ++i)
        {
          Segment* s = new QuadBez(
                      DoublePoint(ctrlPts[i*2]),
                      DoublePoint(ctrlPts[i*2+1]),
                      DoublePoint(ctrlPts[i*2+2]),
                      ref, i+1, 1, precision);
          segments.push_back(s);
        }
        break;
    }
  };
  //------------------------------------------------------------------------------

  void Bezier::Clear()
  {
    for (size_t i = 0; i < segments.size(); ++i)
      delete segments[i];
    segments.resize(0);
  };
  //------------------------------------------------------------------------------

  Bezier::~Bezier()
  {
    Clear();
  };
  //------------------------------------------------------------------------------

  void Bezier::FlattenedPath(ClipperLib::Polygon& out_poly)
  {
    out_poly.resize(0);
    for (size_t i = 0; i < segments.size(); ++i)
      segments[i]->GetFlattenedPath(out_poly, i == 0);
  }

//------------------------------------------------------------------------------

  void Bezier::Reconstruct(long64 startZ, long64 endZ, ClipperLib::Polygon& out_poly)
  {
    out_poly.resize(0);
    BezierType bt1, bt2;
    unsigned short seg1, seg2;
    unsigned short  ref1, ref2;
    unsigned startZu = UnMakeZ(startZ, bt1, seg1, ref1);
    unsigned endZu   = UnMakeZ(endZ,   bt2, seg2, ref2);

    if (bt1 != beziertype || bt1 != bt2 ||
      ref1 != reference || ref1 != ref2) return;

    if (seg1 >= segments.size() || seg2 >= segments.size()) return;

    //check orientation because it's much simpler to temporarily unreverse when
    //the startIdx and endIdx are reversed ...
    bool reversed = (seg1 > seg2);
    if (reversed)
    {
      unsigned i = seg1;
      seg1 = seg2;
      seg2 = i;
      i = startZu;
      startZu = endZu;
      endZu = i;
    }

    //do further checks for reversal, in case reversal within a single segment ...
    if (!reversed && seg1 == seg2 && startZ != 1 && endZ != 1)
    {
      unsigned i = GetMostSignificantBit(startZu);
      unsigned j = GetMostSignificantBit(endZu);
      unsigned k = std::max(i, j);
      //nb: we must compare Node indexes at the same level ...
      i = startZu << (k - i);
      j = endZu << (k - j);
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
      IntNode* intList = new IntNode(0);
      IntNode* intCurr = intList;

      if (seg1 != seg2)
        ReconstructInternal(seg1, startZu, 1, intCurr);
      else
        ReconstructInternal(seg1, startZu, endZu, intCurr);

      //IntList now contains the indexes of one or a series of sub-segments
      //that together define part of or the whole of the original segment.
      //We now append these sub-segments to the new list of control points ...

      intCurr = intList->next; //nb: skips the dummy IntNode
      while (intCurr)
      {
        Segment* s = segments[seg1];
        int j = intCurr->val;
        int k = GetMostSignificantBit(j) -1;
        while (k >= 0)
        {
          if (!s->Childs[0]) break;
          if (IsBitSet(j, k--))
            s = s->Childs[1]; 
          else
            s = s->Childs[0];
        }
        AddCtrlPoint(s, out_poly);
        intCurr = intCurr->next;
      } //while 

      DisposeIntNodes(intList);
      seg1++;
      startZu = 1;
    }
    if (reversed)
      ReversePolygon(out_poly);
  };
  //------------------------------------------------------------------------------

  void Bezier::ReconstructInternal(unsigned short segIdx, unsigned startIdx, unsigned endIdx, IntNode* intCurr)
  {
    //get the maximum level ...
    unsigned L1 = GetMostSignificantBit(startIdx);
    unsigned L2 = GetMostSignificantBit(endIdx);
    int Level = std::max(L1, L2);

    if (Level == 0) 
    {
      InsertInt(intCurr, 1);
      return;
    }

    int L, R;
    //Right marker (R): EndIdx projected onto the bottom level ...
    if (endIdx == 1) 
    {
      R = (1 << (Level +1)) - 1;
    } else
    {
      int j = (Level - L2);
      R = (endIdx << j) + (1 << j) -1;
    }

    if (startIdx == 1) //special case
    {
      //Left marker (L) is bottom left of the binary tree ...
      L = (1 << Level);
      L1 = Level;
    } else
    {
      //For any given Z value, its corresponding X & Y coords (created by
      //FlattenPath using De Casteljau's algorithm) refered to the ctrl[3] coords
      //of many tiny polybezier segments. Since ctrl[3] coords are identical to
      //ctrl[0] coords in the following node, we can safely increment StartIdx ...
      L = startIdx +1;
      if (L == (1 << (Level +1))) return; //loops around tree so already at the end
    }

    //now get blocks of nodes from the LEFT ...
    int j = Level - L1;
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
    } while (L != (3 << (Level - j - 1)) && //ie crosses the ditch in the middle
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
      } while (R != (3 << (Level - j)) -1 && ((R << j) > L));
    
  };
  //------------------------------------------------------------------------------

} //end namespace
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

#ifndef beziers_hpp
#define beziers_hpp

#include <vector>
#include "clipper.hpp"

namespace BezierLib {

  using namespace ClipperLib;

  enum BezierType {CubicBezier, QuadBezier};

  struct IntNode;
  class Segment;

  class Bezier
  {
  private:
    int reference;
    BezierType beziertype;
    //segments: ie supports poly-beziers (ie before flattening) with up to 16,383 segments 
    std::vector< Segment* > segments;
    void ReconstructInternal(unsigned short segIdx, unsigned startIdx, unsigned endIdx, IntNode* intCurr);
  public:
    Bezier(){};
    Bezier(
      const ClipperLib::Polygon& ctrlPts,  //CtrlPts: Bezier control points
      BezierType beztype,                  //CubicBezier or QuadBezier ...
      short ref,                           //Ref: user supplied identifier;
      double precision = 0.5               //Precision of flattened path
      );
    ~Bezier();
    void Clear();
    void SetCtrlPoints(const ClipperLib::Polygon& ctrlPts,
      BezierType beztype, unsigned short ref, double precision = 0.5);
    void FlattenedPath(ClipperLib::Polygon& out_poly);
    //Reconstruct: returns a list of Bezier control points using the
    //information provided in the startZ and endZ parameters (together with
    //the object's stored data) ...
    void Reconstruct(long64 startZ, long64 endZ, ClipperLib::Polygon& out_poly); //Control points again.
  };

} //BezierLib namespace
#endif //bezier_hpp

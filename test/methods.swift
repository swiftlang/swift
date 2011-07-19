// RUN: %swift %s -verify

import swift

//===----------------------------------------------------------------------===//
// Test imported names
//===----------------------------------------------------------------------===//

struct ostream {}  // simplified :-)

struct rect { width : int, height : int }

func area(r : rect) -> int = r.width*r.height
func print(r : rect) -> (os : ostream) -> void {
// os << r.width << ", " << r.height << " area=" << r.area;
}



func test1(os : ostream) {
  var x : rect
  x.print(os)
}

meth rect::print(os : ostream) {
//  os << width << ", " << height << " area=" << this.area;
}



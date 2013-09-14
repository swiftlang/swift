import [exported] Foundation

//===----------------------------------------------------------------------===//
// NSRect
//===----------------------------------------------------------------------===//

extension NSRect {
  constructor(x : CGFloat, y : CGFloat, width : CGFloat, height : CGFloat) {
    origin = CGPoint(x, y)
    size = CGSize(width, height)
  }
}


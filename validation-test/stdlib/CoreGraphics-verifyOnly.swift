// RUN: %target-typecheck-verify-swift
// REQUIRES: objc_interop

import CoreGraphics

//===----------------------------------------------------------------------===//
// CGColorSpace
//===----------------------------------------------------------------------===//

// CGColorSpace.colorTable
// TODO: has memory issues as a runtime test, so make it verify-only for now
let table: [UInt8] = [0,0,0, 255,0,0, 0,0,255, 0,255,0,
 255,255,0, 255,0,255, 0,255,255, 255,255,255]
let space = CGColorSpace(indexedBaseSpace: CGColorSpaceCreateDeviceRGB(),
 last: table.count - 1, colorTable: table)!
// expectOptionalEqual(table, space.colorTable)

//===----------------------------------------------------------------------===//
// CGContext
//===----------------------------------------------------------------------===//

func testCGContext(context: CGContext, image: CGImage, glyph: CGGlyph) {
  
  context.setLineDash(phase: 0.5, lengths: [0.1, 0.2]) 

  context.move(to: CGPoint.zero) 

  context.addLine(to: CGPoint(x: 0.5, y: 0.5)) 

  context.addCurve(to: CGPoint(x: 1, y: 1), control1: CGPoint(x: 1, y: 0), control2: CGPoint(x: 0, y: 1)) 

  context.addQuadCurve(to: CGPoint(x: 0.5, y: 0.5), control: CGPoint(x: 0.5, y: 0)) 

  context.addRects([CGRect(x: 0, y: 0, width: 100, height: 100)]) 

  context.addLines(between: [CGPoint(x: 0.5, y: 0.5)]) 

  context.addArc(center: CGPoint(x: 0.5, y: 0.5), radius: 1, startAngle: 0, endAngle: .pi, clockwise: false) 

  context.addArc(tangent1End: CGPoint(x: 1, y: 1), tangent2End: CGPoint(x: 0.5, y: 0.5), radius: 0.5) 

  context.fill([CGRect(x: 0, y: 0, width: 100, height: 100)])
  
  context.fillPath()
  context.fillPath(using: .evenOdd)

  context.strokeLineSegments(between: [CGPoint(x: 0.5, y: 0.5), CGPoint(x: 0, y: 0.5)]) 

  context.clip(to: [CGRect(x: 0, y: 0, width: 100, height: 100)]) 
  
  context.clip()
  context.clip(using: .evenOdd)

  context.draw(image, in: CGRect(x: 0, y: 0, width: 100, height: 100), byTiling: true) 

  print(context.textPosition) 

  context.showGlyphs([glyph], at: [CGPoint(x: 0.5, y: 0.5)]) 

}

//===----------------------------------------------------------------------===//
// CGDirectDisplay
//===----------------------------------------------------------------------===//

#if os(macOS)
let (dx, dy) = CGGetLastMouseDelta()
#endif

//===----------------------------------------------------------------------===//
// CGImage
//===----------------------------------------------------------------------===//

func testCGImage(image: CGImage) -> CGImage? {
  return image.copy(maskingColorComponents: [1, 0, 0])
}

//===----------------------------------------------------------------------===//
// CGLayer
//===----------------------------------------------------------------------===//

func testDrawLayer(in context: CGContext) {
  let layer = CGLayer(context, size: CGSize(width: 512, height: 384),
   auxiliaryInfo: nil)!
    context.draw(layer, in: CGRect(origin: .zero, size: layer.size))
    context.draw(layer, at: CGPoint(x: 20, y: 20))
}

func testCGPath(path: CGPath) {
  
  let dashed = path.copy(dashingWithPhase: 1, lengths: [0.2, 0.3, 0.5])
  
  let stroked = path.copy(strokingWithWidth: 1, lineCap: .butt,
   lineJoin: .miter, miterLimit: 0.1)

  let mutable = stroked.mutableCopy()!
  
  // test inferred transform parameter for all below
  print(path.contains(CGPoint(x: 0.5, y: 0.5)))
  print(path.contains(CGPoint(x: 0.5, y: 0.5), using: .evenOdd))

  mutable.move(to: .zero) 

  mutable.addLine(to: CGPoint(x: 0.5, y: 0.5)) 

  mutable.addCurve(to: CGPoint(x: 1, y: 1), control1: CGPoint(x: 1, y: 0), control2: CGPoint(x: 0, y: 1)) 

  mutable.addQuadCurve(to: CGPoint(x: 0.5, y: 0.5), control: CGPoint(x: 0.5, y: 0)) 

  mutable.addRect(CGRect(x: 0, y: 0, width: 10, height: 10)) 
  mutable.addRects([CGRect(x: 0, y: 0, width: 100, height: 100)]) 

  mutable.addLines(between: [CGPoint(x: 0.5, y: 0.5)]) 

  mutable.addEllipse(in: CGRect(x: 0, y: 0, width: 50, height: 70))
  
  mutable.addArc(center: CGPoint(x: 0.5, y: 0.5), radius: 1, startAngle: 0, endAngle: .pi, clockwise: false) 

  mutable.addArc(tangent1End: CGPoint(x: 1, y: 1), tangent2End: CGPoint(x: 0.5, y: 0.5), radius: 0.5) 

  mutable.addRelativeArc(center: CGPoint(x: 1, y: 1), radius: 0.5,
   startAngle: .pi, delta: .pi/2)
  
  mutable.addPath(dashed)
  
}

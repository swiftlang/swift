// RUN: %target-swift-frontend -emit-ir -O %s | FileCheck %s

// Test some imported CG APIs
import CoreGraphics

// REQUIRES: OS=macosx

// Get a transform that will rotate around a given offset
public func rotationAround(offset: CGPoint, angle: CGFloat,
        transform: CGAffineTransform = .identity) -> CGAffineTransform {
  // FIXME: consistent API namings
  return transform.translateBy(x: offset.x, y: offset.y)
                  .rotate(angle)
                  .translateBy(x: -offset.x, y: -offset.y)

// CHECK:   call void @CGAffineTransformTranslate(%{{.*}}.CGAffineTransform* {{.*}}, %{{.*}}.CGAffineTransform* {{.*}},{{.*}}, {{.*}})
// CHECK:   call void @CGAffineTransformRotate(%{{.*}}.CGAffineTransform* {{.*}}, %{{.*}}.CGAffineTransform* {{.*}},{{.*}})
// CHECK:   call void @CGAffineTransformTranslate(%{{.*}}.CGAffineTransform* {{.*}}, %{{.*}}.CGAffineTransform* {{.*}},{{.*}}, {{.*}})
}

// Trace a path in red
public func trace(in context: CGContext, path: CGPath) {
  let red = CGColor(red: 1, green: 0, blue: 0, alpha: 1)
  context.saveGState()
  context.addPath(path)
  context.setStrokeColor(red)
  context.strokePath()
  context.restoreGState()
// CHECK:   call %{{.*}}.CGColor* @CGColorCreateGenericRGB(double 1.000000e+00, double 0.000000e+00, double 0.000000e+00, double 1.000000e+00)
// CHECK:   call void @CGContextSaveGState(%{{.*}}.CGContext* %{{.*}})
// CHECK:   call void @CGContextAddPath(%{{.*}}.CGContext* %{{.*}}, %{{.*}}.CGPath* %{{.*}})
// CHECK:   call void @CGContextSetStrokeColorWithColor(%{{.*}}.CGContext* %{{.*}}, %{{.*}}.CGColor* %{{.*}})
// CHECK:   call void @CGContextStrokePath(%{{.*}}.CGContext* %{{.*}})
// CHECK:   call void @CGContextRestoreGState(%{{.*}}.CGContext* %{{.*}})
}

public func pdfOperations(_ context: CGContext) {
	context.beginPDFPage(nil)
	context.endPDFPage()
	context.closePDF()
// CHECK:   call void @CGPDFContextBeginPage(%{{.*}}.CGContext* %{{.*}}, %{{.*}}.__CFDictionary* {{.*}})
// CHECK:   call void @CGPDFContextEndPage(%{{.*}}.CGContext* %{{.*}})
// CHECK:   call void @CGPDFContextClose(%{{.*}}.CGContext* %{{.*}})

}


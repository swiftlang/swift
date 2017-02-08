//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// Redeclarations of all SwiftPrivate symbols with appropriate markup,
// so that tools can help with migration

// @available(*, unavailable, renamed:"DispatchQueue.init(label:attributes:target:)")
@available(*, unavailable, message:"Use == instead")
public func CGAffineTransformEqualToTransform(_ t1: CGAffineTransform, _ t2: CGAffineTransform) -> Bool
  { fatalError() }

@available(*, unavailable, message:"Use class var white/black/clear instead")
public func CGColorGetConstantColor(_ colorName: CFString?) -> CGColor? 
  { fatalError() }

@available(*, unavailable, message:"Use == instead")
public func CGColorEqualToColor(_ color1: CGColor?, _ color2: CGColor?) -> Bool 
  { fatalError() }

@available(*, unavailable, renamed:"getter:CGColor.components(self:)")
public func CGColorGetComponents(_ color: CGColor?) -> UnsafePointer<CGFloat> 
  { fatalError() }

@available(*, unavailable, message:"Use colorTable.count instead")
public func CGColorSpaceGetColorTableCount(_ space: CGColorSpace?) -> Int 
  { fatalError() }

@available(*, unavailable, renamed:"CGColorSpace.colorTable(self:_:)")
public func CGColorSpaceGetColorTable(_ space: CGColorSpace?, _ table: UnsafeMutablePointer<UInt8>) 
  { fatalError() }

@available(*, unavailable, message:"Use setLineDash(self:phase:lengths:)")
public func CGContextSetLineDash(_ c: CGContext?, _ phase: CGFloat, _ lengths: UnsafePointer<CGFloat>, _ count: Int) 
  { fatalError() }

@available(*, unavailable, message:"Use move(to:) instead")
public func CGContextMoveToPoint(_ c: CGContext?, _ x: CGFloat, _ y: CGFloat) 
  { fatalError() }

@available(*, unavailable, message:"Use addLine(to:) instead")
public func CGContextAddLineToPoint(_ c: CGContext?, _ x: CGFloat, _ y: CGFloat) 
  { fatalError() }

@available(*, unavailable, message:"Use addCurve(to:control1:control2:) instead")
public func CGContextAddCurveToPoint(_ c: CGContext?, _ cp1x: CGFloat, _ cp1y: CGFloat, _ cp2x: CGFloat, _ cp2y: CGFloat, _ x: CGFloat, _ y: CGFloat) 
  { fatalError() }

@available(*, unavailable, message:"Use addQuadCurve(to:control:)")
public func CGContextAddQuadCurveToPoint(_ c: CGContext?, _ cpx: CGFloat, _ cpy: CGFloat, _ x: CGFloat, _ y: CGFloat) 
  { fatalError() }

@available(*, unavailable, message:"Use addRects(_:)")
public func CGContextAddRects(_ c: CGContext?, _ rects: UnsafePointer<CGRect>, _ count: Int) 
  { fatalError() }

@available(*, unavailable, message:"Use addLines(between:)")
public func CGContextAddLines(_ c: CGContext?, _ points: UnsafePointer<CGPoint>, _ count: Int) 
  { fatalError() }

@available(*, unavailable, message:"Use addArc(center:radius:startAngle:endAngle:clockwise:)")
public func CGContextAddArc(_ c: CGContext?, _ x: CGFloat, _ y: CGFloat, _ radius: CGFloat, _ startAngle: CGFloat, _ endAngle: CGFloat, _ clockwise: Int32) 
  { fatalError() }

@available(*, unavailable, message:"Use addArc(self:x1:y1:x2:y2:radius:)")
public func CGContextAddArcToPoint(_ c: CGContext?, _ x1: CGFloat, _ y1: CGFloat, _ x2: CGFloat, _ y2: CGFloat, _ radius: CGFloat) 
  { fatalError() }

@available(*, unavailable, message:"Use fill(self:_:count:)")
public func CGContextFillRects(_ c: CGContext?, _ rects: UnsafePointer<CGRect>, _ count: Int) 
  { fatalError() }

@available(*, unavailable, message:"Use strokeLineSegments(self:between:count:)")
public func CGContextStrokeLineSegments(_ c: CGContext?, _ points: UnsafePointer<CGPoint>, _ count: Int) 
  { fatalError() }

@available(*, unavailable, message:"Use clip(to:)")
public func CGContextClipToRects(_ c: CGContext?, _ rects: UnsafePointer<CGRect>, _ count: Int) 
  { fatalError() }

@available(*, unavailable, message:"Use draw(_:in:)")
public func CGContextDrawImage(_ c: CGContext?, _ rect: CGRect, _ image: CGImage?) 
  { fatalError() }

@available(*, unavailable, message:"Use draw(_:in:byTiling:)")
public func CGContextDrawTiledImage(_ c: CGContext?, _ rect: CGRect, _ image: CGImage?) 
  { fatalError() }

@available(*, unavailable, renamed:"getter:CGContext.textPosition(self:)")
public func CGContextGetTextPosition(_ c: CGContext?) -> CGPoint 
  { fatalError() }

@available(*, unavailable, message:"Use var textPosition")
public func CGContextSetTextPosition(_ c: CGContext?, _ x: CGFloat, _ y: CGFloat) 
  { fatalError() }

@available(*, unavailable, message:"Use showGlyphs(_:at:)")
public func CGContextShowGlyphsAtPositions(_ c: CGContext?, _ glyphs: UnsafePointer<CGGlyph>, _ Lpositions: UnsafePointer<CGPoint>, _ count: Int) 
  { fatalError() }

@available(*, unavailable, renamed:"CGContext.fillPath(self:)")
public func CGContextFillPath(_ c: CGContext?) 
  { fatalError() }

@available(*, unavailable, message:"Use fillPath(using:)")
public func CGContextEOFillPath(_ c: CGContext?) 
  { fatalError() }

@available(*, unavailable, renamed:"CGContext.clip(self:)")
public func CGContextClip(_ c: CGContext?) 
  { fatalError() }

@available(*, unavailable, message:"Use clip(using:)")
public func CGContextEOClip(_ c: CGContext?) 
  { fatalError() }

@available(*, unavailable, renamed:"CGGetLastMouseDelta") // different type
public func CGGetLastMouseDelta(_ deltaX: UnsafeMutablePointer<Int32>?, _ deltaY: UnsafeMutablePointer<Int32>?)
  { fatalError() }

@available(*, unavailable, message:"Use divided(atDistance:from:)")
public func CGRectDivide(_ rect: CGRect, _ slice: UnsafeMutablePointer<CGRect>, _ remainder: UnsafeMutablePointer<CGRect>, _ amount: CGFloat, _ edge: CGRectEdge) 
  { fatalError() }

@available(*, unavailable, message:"Use CGPoint.init(dictionaryRepresentation:)")
public func CGPointMakeWithDictionaryRepresentation(_ dict: CFDictionary?, _ point: UnsafeMutablePointer<CGPoint>) -> Bool 
  { fatalError() }

@available(*, unavailable, message:"Use CGSize.init(dictionaryRepresentation:)")
public func CGSizeMakeWithDictionaryRepresentation(_ dict: CFDictionary?, _ size: UnsafeMutablePointer<CGSize>) -> Bool 
  { fatalError() }

@available(*, unavailable, message:"Use CGRect.init(dictionaryRepresentation:)")
public func CGRectMakeWithDictionaryRepresentation(_ dict: CFDictionary?, _ rect: UnsafeMutablePointer<CGRect>) -> Bool 
  { fatalError() }

@available(*, unavailable, renamed:"CGImage.copy(self:maskingColorComponents:)")
public func CGImageCreateWithMaskingColors(_ image: CGImage?, _ components: UnsafePointer<CGFloat>) -> CGImage? 
  { fatalError() }

@available(*, unavailable, message:"Use draw(_:in:)")
public func CGContextDrawLayerInRect(_ context: CGContext?, _ rect: CGRect, _ layer: CGLayer?) 
  { fatalError() }

@available(*, unavailable, message:"Use draw(_:at:)")
public func CGContextDrawLayerAtPoint(_ context: CGContext?, _ point: CGPoint, _ layer: CGLayer?) 
  { fatalError() }

@available(*, unavailable, message:"Use copy(byDashingWithPhase:lengths:transform:)")
public func CGPathCreateCopyByDashingPath(_ path: CGPath?, _ transform: UnsafePointer<CGAffineTransform>, _ phase: CGFloat, _ lengths: UnsafePointer<CGFloat>, _ count: Int) -> CGPath? 
  { fatalError() }

@available(*, unavailable, message:"Use copy(byStroking:lineWidth:lineCap:lineJoin:miterLimit:transform:)")
public func CGPathCreateCopyByStrokingPath(_ path: CGPath?, _ transform: UnsafePointer<CGAffineTransform>, _ lineWidth: CGFloat, _ lineCap: CGLineCap, _ lineJoin: CGLineJoin, _ miterLimit: CGFloat) -> CGPath? 
  { fatalError() }

@available(*, unavailable, message:"Use == instead")
public func CGPathEqualToPath(_ path1: CGPath?, _ path2: CGPath?) -> Bool 
  { fatalError() }

@available(*, unavailable, message:"Use move(to:transform:)")
public func CGPathMoveToPoint(_ path: CGMutablePath?, _ m: UnsafePointer<CGAffineTransform>, _ x: CGFloat, _ y: CGFloat) 
  { fatalError() }

@available(*, unavailable, message:"Use addLine(to:transform:)")
public func CGPathAddLineToPoint(_ path: CGMutablePath?, _ m: UnsafePointer<CGAffineTransform>, _ x: CGFloat, _ y: CGFloat) 
  { fatalError() }

@available(*, unavailable, message:"Use addCurve(to:control1:control2:transform:)")
public func CGPathAddCurveToPoint(_ path: CGMutablePath?, _ m: UnsafePointer<CGAffineTransform>, _ cp1x: CGFloat, _ cp1y: CGFloat, _ cp2x: CGFloat, _ cp2y: CGFloat, _ x: CGFloat, _ y: CGFloat) 
  { fatalError() }

@available(*, unavailable, message:"Use addQuadCurve(to:control:transform:)")
public func CGPathAddQuadCurveToPoint(_ path: CGMutablePath?, _ m: UnsafePointer<CGAffineTransform>, _ cpx: CGFloat, _ cpy: CGFloat, _ x: CGFloat, _ y: CGFloat) 
  { fatalError() }

@available(*, unavailable, message:"Use addRect(_:transform:)")
public func CGPathAddRect(_ path: CGMutablePath?, _ m: UnsafePointer<CGAffineTransform>, _ rect: CGRect) 
  { fatalError() }

@available(*, unavailable, message:"Use addRects(_:transform:)")
public func CGPathAddRects(_ path: CGMutablePath?, _ m: UnsafePointer<CGAffineTransform>, _ rects: UnsafePointer<CGRect>, _ count: Int) 
  { fatalError() }

@available(*, unavailable, message:"Use addLines(between:transform:)")
public func CGPathAddLines(_ path: CGMutablePath?, _ m: UnsafePointer<CGAffineTransform>, _ points: UnsafePointer<CGPoint>, _ count: Int) 
  { fatalError() }

@available(*, unavailable, message:"Use addEllipse(rect:transform:)")
public func CGPathAddEllipseInRect(_ path: CGMutablePath?, _ m: UnsafePointer<CGAffineTransform>, _ rect: CGRect) 
  { fatalError() }

@available(*, unavailable, message:"Use addRelativeArc(center:radius:startAngle:delta:transform:)")
public func CGPathAddRelativeArc(_ path: CGMutablePath?, _ matrix: UnsafePointer<CGAffineTransform>, _ x: CGFloat, _ y: CGFloat, _ radius: CGFloat, _ startAngle: CGFloat, _ delta: CGFloat) 
  { fatalError() }

@available(*, unavailable, message:"Use addArc(center:radius:startAngle:endAngle:clockwise:transform:)")
public func CGPathAddArc(_ path: CGMutablePath?, _ m: UnsafePointer<CGAffineTransform>, _ x: CGFloat, _ y: CGFloat, _ radius: CGFloat, _ startAngle: CGFloat, _ endAngle: CGFloat, _ clockwise: Bool) 
  { fatalError() }

@available(*, unavailable, message:"Use addArc(tangent1End:tangent2End:radius:transform:)")
public func CGPathAddArcToPoint(_ path: CGMutablePath?, _ m: UnsafePointer<CGAffineTransform>, _ x1: CGFloat, _ y1: CGFloat, _ x2: CGFloat, _ y2: CGFloat, _ radius: CGFloat) 
  { fatalError() }

@available(*, unavailable, message:"Use addPath(_:transform:)")
public func CGPathAddPath(_ path1: CGMutablePath?, _ m: UnsafePointer<CGAffineTransform>, _ path2: CGPath?) 
  { fatalError() }

@available(*, unavailable, message:"Use CGColor.white") // retyped
public var kCGColorWhite: CFString 
  { fatalError() }

@available(*, unavailable, message:"Use CGColor.black") // retyped
public var kCGColorBlack: CFString 
  { fatalError() }

@available(*, unavailable, message:"Use CGColor.clear") // retyped
public var kCGColorClear: CFString 
  { fatalError() }

// TODO: also add migration support from previous Swift3 betas?

// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop
// REQUIRES: OS=macosx
import CoreGraphics
import StdlibUnittest

//===----------------------------------------------------------------------===//
// CGAffineTransform
//===----------------------------------------------------------------------===//

// Equatable conformance via CGAffineTransformEqualToTransform
let bourne = CGAffineTransform(a: 1, b: 0, c: 0, d: 1, tx: 0, ty: 0)
expectEqual(bourne, CGAffineTransform.identity)

//===----------------------------------------------------------------------===//
// CGColor
//===----------------------------------------------------------------------===//

// CGColor.components
let red = CGColor(red: 1, green: 0, blue: 0, alpha: 1)
let components = red.components!
expectEqual(components.count, 4)
expectEqual(components[0], 1)
expectEqual(components[1], 0)
expectEqual(components[2], 0)
expectEqual(components[3], 1)

// CGColor Equatable conformance via CGColorEqualToColor
let 赤 = CGColor(red: 1, green: 0, blue: 0, alpha: 1)
expectEqual(red, 赤)

//===----------------------------------------------------------------------===//
// CGGeometry
//===----------------------------------------------------------------------===//

// other tests elsewhere for basics, only the new stuff here
let point = CGPoint(x: 1, y: 2)
let pDict = point.dictionaryRepresentation
let newPoint = CGPoint(dictionaryRepresentation: pDict)
expectOptionalEqual(point, newPoint)

let size = CGSize(width: 3, height: 4)
let sDict = size.dictionaryRepresentation
let newSize = CGSize(dictionaryRepresentation: sDict)
expectOptionalEqual(size, newSize)

let rect = CGRect(origin: point, size: size)
let rDict = rect.dictionaryRepresentation
let newRect = CGRect(dictionaryRepresentation: rDict)
expectOptionalEqual(rect, newRect)

// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -parse -verify -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s
// RUN: ls -lR %t/clang-module-cache | grep ctypes.pcm

import ctypes

var pt : CGPoint 

func testColor() {
  var c : Color = red
  c = blue
}

func testTribool() {
  var b = Indeterminate
  b = True
}

func testAnonEnum() {
  var a = AnonConst1
  a = AnonConst2
  var a2 : Int = a
}

func testPoint() -> Float {
  var p : Point
  p.x = 1.0
  return p.y
}

func testAnonStructs() {
  var a_s : AnonStructs
  a_s.a = 5
  a_s.b = 3.14
  a_s.c = 7.5
}

// FIXME: Import arrays as real array-looking things.

func testArrays() {
  var fes : NSFastEnumerationState
  var ulong : CUnsignedLong
  var pulong : UnsafePointer<CUnsignedLong>

  ulong = fes.state
  pulong = fes.mutationsPtr
  ulong = fes.extra.0
  ulong = fes.extra.1
  ulong = fes.extra.2
  ulong = fes.extra.3
  ulong = fes.extra.4
}

// FIXME: Import pointers to opaque types as unique types.

func testPointers() {
  var hWnd : HWND = HWND.null()
  var cfty : CFTypeRef = CFTypeRef.null()
  var cfstr : CFStringRef = CFStringRef.null()
}

// Ensure that imported structs can be extended, even if typedef'ed on the C
// side.

func sqrt(x:Float) -> Float {}
func atan2(x:Float, y:Float) -> Float {}

extension Point {
  func asPolar() -> (rho:Float, theta:Float) {
    return (sqrt(x*x + y*y), atan2(x, y))
  }
}

extension AnonStructs {
  func frob() -> Double {
    return Double(a) + Double(b) + c
  }
}

extension NSFastEnumerationState {
  func reset() {}
}

extension NSRect {
  constructor(x:Double, y:Double, w:Double, h:Double) {
    origin.x = x
    origin.y = y
    size.width = w
    size.height = h
  }
}

extension CGRect {
  func printAsX11Geometry() {
    print("\(size.width)x\(size.height)+\(origin.x)+\(origin.y)")
  }
}

func testFuncStructDisambiguation() {
  var a : funcOrStruct
  funcOrStruct()
}


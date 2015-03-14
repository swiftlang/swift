// RUN: rm -rf %t && mkdir %t
// RUN: %build-irgen-test-overlays
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) %s -emit-ir | FileCheck %s

// REQUIRES: CPU=x86_64
// REQUIRES: objc_interop

import ObjectiveC
import Foundation
import gizmo

@objc protocol P1 {}
@objc protocol P2 {}
@objc protocol P3 {}

@objc class Methods {
  @objc func testSizedUnsignedTypes(a: UInt8, b: UInt16, c: UInt32, d: UInt64) {}
// CHECK: private unnamed_addr constant [20 x i8] c"v36@0:8C16S20I24Q28\00"

  @objc func testSizedFloats(a: Float32, b: Float64) {}
// CHECK: private unnamed_addr constant [14 x i8] c"v28@0:8f16d20\00"

  @objc func testParens(a: ((Int))) {}
// CHECK: private unnamed_addr constant [11 x i8] c"v24@0:8q16\00"

  @objc func testPrimitives(b: CBool, i: Int, f: Float, d: Double)
    -> COpaquePointer { return COpaquePointer() }
// CHECK: private unnamed_addr constant [21 x i8] c"^v40@0:8B16q20f28d32\00"

  @objc func testCSignedTypes(a: CSignedChar, b: CShort, c: CInt, d: CLong, e: CLongLong) {}
// CHECK: private unnamed_addr constant [23 x i8] c"v44@0:8c16s20i24q28q36\00"

  @objc func testCUnsignedTypes(a: CUnsignedChar, b: CUnsignedShort, c: CUnsignedInt, d: CUnsignedLong, e: CUnsignedLongLong) {}
// CHECK: private unnamed_addr constant [23 x i8] c"v44@0:8C16S20I24Q28Q36\00"

  @objc func testCChars(basic: CChar, wchar wide: CWideChar, char16: CChar16, char32: CChar32) {}
// CHECK: private unnamed_addr constant [20 x i8] c"v32@0:8c16i20S24i28\00"

  @objc func testCBool(a: CBool) {}
// CHECK: private unnamed_addr constant [11 x i8] c"v20@0:8B16\00"

  @objc func testSizedSignedTypes(a: Int8, b: Int16, c: Int32, d: Int64) {}
// CHECK: private unnamed_addr constant [20 x i8] c"v36@0:8c16s20i24q28\00"

  @objc class func getSelf() -> Methods.Type { return self }
  // CHECK: private unnamed_addr constant [8 x i8] c"#16@0:8\00"

  @objc func getDynamicSelf() -> Self { return self }
  // CHECK: private unnamed_addr constant [8 x i8] c"@16@0:8\00"

func testId(s: AnyObject) -> AnyObject { return self }
// CHECK: private unnamed_addr constant [11 x i8] c"@24@0:8@16\00"

@objc func comp1(a: protocol<P1, P2>, b: protocol<P1, P2, P3>) -> protocol<P1,P2> { return a }
// CHECK: private unnamed_addr constant [14 x i8] c"@32@0:8@16@24\00"

@objc func returnsBool(b : Bool) -> Bool { return b } 
// CHECK: private unnamed_addr constant [11 x i8] c"B20@0:8B16\00"

@objc func comp1(a: Methods, b: Methods, c: Methods) -> Methods { return a }
// CHECK: private unnamed_addr constant [17 x i8] c"@40@0:8@16@24@32\00"

 @objc func passSelector(aSelector : Selector) {}
// CHECK: private unnamed_addr constant [11 x i8] c"v24@0:8:16\00"

@objc func copyUnsafeMutablePointer(p: UnsafeMutablePointer<Int32>) -> UnsafeMutablePointer<Int32> { return p }
// CHECK: private unnamed_addr constant [13 x i8] c"^i24@0:8^i16\00"

@objc func copyUnsafeMutablePointerInt(p: UnsafeMutablePointer<Int>) -> UnsafeMutablePointer<Int> { return p }
// CHECK: private unnamed_addr constant [13 x i8] c"^q24@0:8^q16\00"

func testArchetype(work: P3) {
}
// CHECK: private unnamed_addr constant [11 x i8] c"v24@0:8@16\00"

@objc func foo(x: Int -> Int) -> Int {
  return 1;
}
// CHECK: private unnamed_addr constant [12 x i8] c"q24@0:8@?16\00"

@objc func returnNSRadixedOptions() -> NSRadixedOptions {
    return .Octal
  }
// CHECK: [[ENUMENCODING:@.*]] = private unnamed_addr constant [8 x i8] c"i16@0:8\00"
@objc func returnChoseNSRadixedOptions(choice: NSRadixedOptions) -> NSRadixedOptions {
    switch choice {
      case .Octal: return .Hex
      case .Hex: return .Octal
    }
  }
// CHECK: private unnamed_addr constant [11 x i8] c"i20@0:8i16\00"

@objc func getRawEnumInGizmo() -> RawEnumInGizmo {
    return InGizmoTwo
  }
// CHECK: { i8* getelementptr inbounds ([18 x i8], [18 x i8]* @"\01L_selector_data(getRawEnumInGizmo)", i64 0, i64 0),
// CHECK: i8* getelementptr inbounds ([8 x i8], [8 x i8]* [[ENUMENCODING]], i64 0, i64 0)

}

// rdar://16308986
@objc class MyCustomObject {
}

@objc protocol MyProtocol {
	func myMethod2(arg : protocol<NSRuncing, NSFunging>)
        func readWithAuthorization(authData : Gizmo, reply : (NSView, NSSpoon) -> Void)
        func doSomething(context: MyCustomObject)
}

// CHECK: [[ENC1:@.*]] = private unnamed_addr constant [35 x i8] c"v24@0:8@\22<NSFunging><NSRuncing>\2216\00"
// CHECK: [[ENC2:@.*]] = private unnamed_addr constant [46 x i8] c"v32@0:8@\22Gizmo\2216@?<v@?@\22NSView\22@\22NSSpoon\22>24\00"
// CHECK: [[ENC3:@.*]] = private unnamed_addr constant [27 x i8] c"v24@0:8@\22MyCustomObject\2216\00"
// CHECK: @_PROTOCOL_METHOD_TYPES__TtP18objc_type_encoding10MyProtocol_ = private constant { [3 x i8*] } { [3 x i8*] [i8* getelementptr inbounds ([35 x i8], [35 x i8]* [[ENC1]], i64 0, i64 0), i8* getelementptr inbounds ([46 x i8], [46 x i8]* [[ENC2]], i64 0, i64 0), i8* getelementptr inbounds ([27 x i8], [27 x i8]* [[ENC3]], i64 0, i64 0)] }

// Check that the compiler emits back-deployed types into their clients, and
// that their metadata and witness tables are unique across images. Run with
// the current MyKit, a MyKit from before the types were introduced, and a
// MyKit that shipped the types before they adopted @backDeployed.

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/old)
// RUN: %empty-directory(%t/shipped)
// RUN: %target-build-swift-dylib(%t/%target-library-name(Common)) -module-name Common %S/Inputs/back_deployed_types/Common.swift -emit-module -emit-module-path %t/Common.swiftmodule
// RUN: %target-build-swift-dylib(%t/%target-library-name(MyKit)) -module-name MyKit %S/Inputs/back_deployed_types/MyKit.swift -enable-library-evolution -emit-module -emit-module-path %t/MyKit.swiftmodule -I %t -L %t -lCommon
// RUN: %target-build-swift-dylib(%t/old/%target-library-name(MyKit)) -module-name MyKit %S/Inputs/back_deployed_types/MyKitOld.swift -enable-library-evolution
// RUN: sed -e '/@backDeployed/d' %S/Inputs/back_deployed_types/MyKit.swift > %t/shipped/MyKit.swift
// RUN: %target-build-swift-dylib(%t/shipped/%target-library-name(MyKit)) -module-name MyKit %t/shipped/MyKit.swift -enable-library-evolution -I %t -L %t -lCommon
// RUN: %target-build-swift-dylib(%t/%target-library-name(A)) -module-name A %S/Inputs/back_deployed_types/Client.swift -I %t -L %t -lCommon -lMyKit -emit-module -emit-module-path %t/A.swiftmodule
// RUN: %target-build-swift-dylib(%t/%target-library-name(B)) -module-name B %S/Inputs/back_deployed_types/Client.swift -I %t -L %t -lCommon -lMyKit -emit-module -emit-module-path %t/B.swiftmodule
// RUN: %target-build-swift %s -I %t -L %t -lA -lB -lCommon -lMyKit -o %t/main %target-rpath(%t)
// RUN: cp %t/main %t/%target-library-name(A) %t/%target-library-name(B) %t/%target-library-name(Common) %t/old/
// RUN: cp %t/main %t/%target-library-name(A) %t/%target-library-name(B) %t/%target-library-name(Common) %t/shipped/
// RUN: %target-codesign %t/main %t/%target-library-name(A) %t/%target-library-name(B) %t/%target-library-name(Common) %t/%target-library-name(MyKit)
// RUN: %target-codesign %t/old/main %t/old/%target-library-name(A) %t/old/%target-library-name(B) %t/old/%target-library-name(Common) %t/old/%target-library-name(MyKit)
// RUN: %target-codesign %t/shipped/main %t/shipped/%target-library-name(A) %t/shipped/%target-library-name(B) %t/shipped/%target-library-name(Common) %t/shipped/%target-library-name(MyKit)
// RUN: %target-run %t/main %t/%target-library-name(A) %t/%target-library-name(B) %t/%target-library-name(Common) %t/%target-library-name(MyKit) | %FileCheck %s --check-prefixes=CHECK,NEW
// RUN: %target-run %t/old/main %t/old/%target-library-name(A) %t/old/%target-library-name(B) %t/old/%target-library-name(Common) %t/old/%target-library-name(MyKit) | %FileCheck %s --check-prefixes=CHECK,OLD
// RUN: %target-run %t/shipped/main %t/shipped/%target-library-name(A) %t/shipped/%target-library-name(B) %t/shipped/%target-library-name(Common) %t/shipped/%target-library-name(MyKit) | %FileCheck %s --check-prefixes=CHECK,NEW

// REQUIRES: executable_test
// REQUIRES: OS=macosx

import Darwin
import MyKit
import Common
import A
import B

func same(_ what: String, _ types: [Any.Type?]) {
  let ids = Set(types.map { $0.map { ObjectIdentifier($0) } })
  let ok = ids.count == 1 && !ids.contains(nil)
  print("\(ok ? "PASS" : "FAIL") \(what)")
}

func check(_ what: String, _ cond: Bool) {
  print("\(cond ? "PASS" : "FAIL") \(what)")
}

// NEW: MyKit version 2
// OLD: MyKit version 1
print("MyKit version \(myKitVersion())")

// The metadata comes from MyKit whenever MyKit provides the type.
func isFromMyKit(_ type: Any.Type) -> Bool {
  let metadata = unsafeBitCast(type, to: UnsafeRawPointer.self)
  let descriptor = metadata.load(fromByteOffset: 8, as: UnsafeRawPointer.self)
  var info = Dl_info()
  guard dladdr(descriptor, &info) != 0, let path = info.dli_fname else {
    return false
  }
  return String(cString: path).hasSuffix("/libMyKit.dylib")
}
// NEW-NEXT: Point metadata from MyKit: true
// OLD-NEXT: Point metadata from MyKit: false
print("Point metadata from MyKit: \(isFromMyKit(Point.self))")
// NEW-NEXT: Box<Int> metadata from MyKit: true
// OLD-NEXT: Box<Int> metadata from MyKit: false
print("Box<Int> metadata from MyKit: \(isFromMyKit(Box<Int>.self))")

// CHECK-NOT: FAIL
// CHECK-NEXT: PASS Box<Int>
same("Box<Int>", [A.Client.boxInt(), B.Client.boxInt(), Box<Int>.self,
                  _typeByName("5MyKit3BoxVySiG")])
// CHECK-NEXT: PASS Either<String, Box<Int>>
same("Either<String, Box<Int>>", [A.Client.either(), B.Client.either(),
                                  Either<String, Box<Int>>.self])
// CHECK-NEXT: PASS Point
same("Point", [A.Client.point(), B.Client.point(), Point.self,
               _typeByName("5MyKit5PointV")])
// CHECK-NEXT: PASS Needs<Box<Int>>
same("Needs<Box<Int>>", [A.Client.needsBoxInt(), B.Client.needsBoxInt(),
                         needsType(Box<Int>.self),
                         needsTypeDynamic(A.Client.makeBox()),
                         needsTypeDynamic(B.Client.makeBox())])
// CHECK-NEXT: PASS Needs<Point>
same("Needs<Point>", [A.Client.needsPoint(), B.Client.needsPoint(),
                      needsType(Point.self),
                      needsTypeDynamic(A.Client.makePoint())])
// CHECK-NEXT: PASS cast
check("cast", (A.Client.makeBox() as? Box<Int>) == Box(42) &&
              (B.Client.makeBox() as? Box<Int>) == Box(42))
// CHECK-NEXT: PASS AnyHashable
check("AnyHashable",
      (A.Client.makePoint() as? AnyHashable) ==
        (B.Client.makePoint() as? AnyHashable))
// CHECK-NEXT: PASS type name
check("type name",
      String(describing: A.Client.either()) == "Either<String, Box<Int>>")
// CHECK-NEXT: PASS Outer.Inner
same("Outer.Inner", [A.Client.inner(), B.Client.inner(), Outer.Inner.self,
                     _typeByName("5MyKit5OuterV5InnerV")])
// CHECK-NEXT: PASS Box<Int>.Nested<String>
same("Box<Int>.Nested<String>", [A.Client.nested(), B.Client.nested(),
                                 Box<Int>.Nested<String>.self])
// CHECK-NEXT: PASS Set of Outer.Inner
if let a = A.Client.makeInner() as? Outer.Inner,
   let b = B.Client.makeInner() as? Outer.Inner {
  check("Set of Outer.Inner", Set([a, b]).count == 1)
} else {
  check("Set of Outer.Inner", false)
}
// CHECK-NEXT: PASS nested type name
check("nested type name",
      String(reflecting: A.Client.nested()) ==
        "MyKit.Box<Swift.Int>.Nested<Swift.String>")

// Witnesses for generic requirements, emitted by each client along with the
// conformances.
// CHECK-NEXT: PASS generic requirement witnesses
check("generic requirement witnesses",
      A.Client.describePoint() == ["1", "2"] &&
      B.Client.describePoint() == ["1", "2"] &&
      A.Client.describeBox() == ["5"] &&
      B.Client.describeEither() == ["7"] &&
      A.Client.countPoint() == 3 &&
      B.Client.makePointFromArray() == Point(x: 3, y: 4) &&
      A.Client.makeBoxFromArray() == B.Client.makeBoxFromArray())
// CHECK-NEXT: PASS dynamic generic requirement witnesses
check("dynamic generic requirement witnesses",
      describeDynamic(A.Client.makePoint()) == ["1", "2"] &&
      describeDynamic(B.Client.makeBox()) == ["42"])
// A conformance to a protocol from MyKit itself.
// CHECK-NEXT: PASS same-module protocol witnesses
check("same-module protocol witnesses",
      A.Client.labelPoint() == "Point(1, 2) 0" &&
      B.Client.labelPoint() == "Point(1, 2) 0" &&
      (A.Client.makePoint() as? any Labeled).map { labelGenerically($0) } ==
        "Point(1, 2) 0")
// CHECK-NEXT: PASS NeedsTransformer<Point>
same("NeedsTransformer<Point>", [A.Client.needsTransformerPoint(),
                                 B.Client.needsTransformerPoint(),
                                 needsTransformer(Point.self),
                                 needsTransformerDynamic(A.Client.makePoint())])
// CHECK-NEXT: PASS NeedsTransformer<Box<Int>>
same("NeedsTransformer<Box<Int>>", [A.Client.needsTransformerBox(),
                                    B.Client.needsTransformerBox(),
                                    needsTransformer(Box<Int>.self),
                                    needsTransformerDynamic(B.Client.makeBox())])
// CHECK-NOT: FAIL

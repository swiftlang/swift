// RUN: %target-swift-frontend -emit-ir %s | %FileCheck %s

// REQUIRES: CPU=x86_64

@objc public enum ExportedToObjC: Int32 {
  case Foo = -1, Bar, Bas
}

// CHECK-LABEL: define{{( protected)?( dllexport)?}} swiftcc i32 @"$s9objc_enum0a1_B7_injectAA14ExportedToObjCOyF"()
// CHECK:         ret i32 -1
public func objc_enum_inject() -> ExportedToObjC {
  return .Foo
}

// CHECK-LABEL: define{{( protected)?( dllexport)?}} swiftcc i64 @"$s9objc_enum0a1_B7_switchySiAA14ExportedToObjCOF"(i32 %0)
// CHECK:         switch i32 %0, label {{%.*}} [
// CHECK:           i32 -1, label {{%.*}}
// CHECK:           i32  0, label {{%.*}}
// CHECK:           i32  1, label {{%.*}}
public func objc_enum_switch(_ x: ExportedToObjC) -> Int {
  switch x {
  case .Foo:
    return 0
  case .Bar:
    return 1
  case .Bas:
    return 2
  }
}


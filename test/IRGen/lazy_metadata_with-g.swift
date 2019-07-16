// RUN: %target-swiftc_driver -parse-as-library -module-name=test -target x86_64-apple-macosx10.15 -wmo -O -g -emit-ir %s  | %FileCheck %s
// REQUIRES: OS=macosx

// Check that the compiler does not emit any metadata for Mystruct and
// Teststruct, even with -g.
// This is also a driver issue, so we are testing with %target-swiftc_driver
// and not just with %target-swift-frontend.

// CHECK: ModuleID

// CHECK-NOT: Mystruct
// CHECK-NOT: Teststruct
// CHECK-NOT: define

// CHECK: DICompileUnit

protocol P {
}

struct Mystruct : P {
}


struct Teststruct {

  static var testvar: some P {
    return Mystruct()
  }
}


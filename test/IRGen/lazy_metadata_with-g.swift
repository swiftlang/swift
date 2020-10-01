// RUN: %target-swiftc_driver -parse-as-library -module-name=test -target %target-cpu-apple-macosx10.15 -wmo -O -g -emit-ir %s  | %FileCheck %s
// REQUIRES: OS=macosx

// Check that the compiler does not emit any metadata for unused internal
// structs and enums, even with -g.
// This is also a driver issue, so we are testing with %target-swiftc_driver
// and not just with %target-swift-frontend.

// CHECK: ModuleID

// CHECK-NOT: Test
// CHECK-NOT: define

// CHECK: DICompileUnit

public protocol P {
  func foo() -> Int
}

struct Teststruct1 : P {
  func foo() -> Int { return 27 }
}


struct Teststruct : P {

  static var testvar: some P {
    switch getit().e {
      case .None:
        return Teststruct1()
      case .View(let v):
        return v
    }
  }

  @inline(never)
  static func getit() -> Teststruct2 {
    return Teststruct2(e: .View(Teststruct1()))
  }

  func foo() -> Int { return 27 }
}

struct Teststruct2 : P {
  enum Testenum {
    case None
    case View(Teststruct1)
  }

  var e: Testenum

  func foo() -> Int { return 27 }
}

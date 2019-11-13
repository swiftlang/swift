// RUN: %empty-directory(%t)
// RUN: %target-build-swift  -o %t/main %target-rpath(%t) %s -swift-version 5
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize_none
// REQUIRES: CPU=arm64 || CPU=arm64e || CPU=x86_64

public protocol Assoc {
  associatedtype A = Int

  @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
  func act() -> A
}


func testAssociatedType<T: Assoc> (_ t: T) {
  print(T.A.self)

}

protocol P {
}

extension Int : P {
}

struct Pair : P {}

struct Test : Assoc {
  @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
  dynamic func act() -> some P {
    return 1
  }
}

extension Test {
  @available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
  @_dynamicReplacement(for: act)
  func act_r() -> some P {
    return Pair()
  }
}

func test() {
  let t = Test()
  // CHECK: Pair
  if #available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *) {
    testAssociatedType(t)
  } else {
    print("Pair")
  }
}

test()

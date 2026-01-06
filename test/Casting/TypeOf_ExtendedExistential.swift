// RUN: %empty-directory(%t)
//
// RUN: %target-build-swift -swift-version 5 -g -Onone  -module-name a %s -o %t/a.swift5.Onone.out
// RUN: %target-codesign %t/a.swift5.Onone.out
// RUN: %target-run %t/a.swift5.Onone.out
//
// RUN: %target-build-swift -swift-version 6 -g -Onone  -module-name a %s -o %t/a.swift6.Onone.out
// RUN: %target-codesign %t/a.swift6.Onone.out
// RUN: %target-run %t/a.swift6.Onone.out
//
// RUN: %target-build-swift -swift-version 5 -g -O  -module-name a %s -o %t/a.swift5.O.out
// RUN: %target-codesign %t/a.swift5.O.out
// RUN: %target-run %t/a.swift5.O.out
//
// RUN: %target-build-swift -swift-version 6 -g -O  -module-name a %s -o %t/a.swift6.O.out
// RUN: %target-codesign %t/a.swift6.O.out
// RUN: %target-run %t/a.swift6.O.out
//
// REQUIRES: executable_test
// REQUIRES: concurrency
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import StdlibUnittest

func blackhole<T>(_ t: T) { }

private func runtimeCast<T,U>(_ from: T, to: U.Type) -> U? {
  return from as? U
}

let CastsTests = TestSuite("Casts")

protocol Box<T> {
  associatedtype T
  var t: T { get }
}

CastsTests.test("type(of:) should look through extended existentials (none)") {
  struct C<T>: Box { var t: T }
  func genericErase<T>(_ value: T) -> Any { value }
  let c: any Box<Int> = C(t: 42)
  if #available(macOS 13.0, iOS 16.0, watchOS 9.0, tvOS 16.0, *) {
    let x = genericErase(c)
    expectEqual("C<Int>", "\(type(of:x))")
  }
}

protocol OBox<T> : AnyObject {
  associatedtype T
  var t: T { get }
}

CastsTests.test("type(of:) should look through extended existentials (class)") {
  class C<T>: OBox {
    var t: T
    init(t: T) { self.t = t }
  }
  func genericErase<T>(_ value: T) -> Any { value }
  let c: any OBox<Int> = C(t: 42)
  if #available(macOS 13.0, iOS 16.0, watchOS 9.0, tvOS 16.0, *) {
    let x = genericErase(c)
    expectEqual("C<Int>", "\(type(of:x))")
  }
}


CastsTests.test("type(of:) should look through extended existentials (metatype)") {
  struct C<T>: Box { var t: T }
  func genericErase<T>(_ value: T) -> Any { value }
  let t: any Box<Int>.Type = C<Int>.self
  if #available(macOS 13.0, iOS 16.0, watchOS 9.0, tvOS 16.0, *) {
    let x = genericErase(t)
    expectEqual("C<Int>.Type", "\(type(of:x))")
  }
}

runAllTests()

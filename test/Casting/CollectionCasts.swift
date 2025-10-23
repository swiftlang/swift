// CollectionCasts.swift - Tests for conversion between collection types.
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
// -----------------------------------------------------------------------------
///
/// Contains tests for non-trapping type conversions reported by users.
///
// -----------------------------------------------------------------------------
// X-RUN: %empty-directory(%t)
//
// RUN: %target-build-swift -swift-version 6 -g -Onone  -module-name a %s -o %t/a.swift6.Onone.out
// RUN: %target-codesign %t/a.swift6.Onone.out
// RUN: %target-run %t/a.swift6.Onone.out
//
// X-RUN: %target-build-swift -swift-version 6 -g -O  -module-name a %s -o %t/a.swift6.O.out
// X-RUN: %target-codesign %t/a.swift6.O.out
// X-RUN: %target-run %t/a.swift6.O.out
//
// X-RUN: %target-build-swift -swift-version 5 -g -Onone  -module-name a %s -o %t/a.swift5.Onone.out
// X-RUN: %target-codesign %t/a.swift5.Onone.out
// X-RUN: %target-run %t/a.swift5.Onone.out
//
// X-RUN: %target-build-swift -swift-version 5 -g -O  -module-name a %s -o %t/a.swift5.O.out
// X-RUN: %target-codesign %t/a.swift5.O.out
// X-RUN: %target-run %t/a.swift5.O.out
//
// REQUIRES: executable_test
// REQUIRES: objc_interop
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import StdlibUnittest
#if _runtime(_ObjC)
import Foundation
#endif

#if _runtime(_ObjC)
@objc
class Root: NSObject {}
#else
class Root {}
#endif

@objc
class Base: Root {
  var k: Int

  init(_ k: Int) {
    self.k = k
  }
}

@objc
class Derived: Base {

}

extension TestSuite {
  @inline(never)
  public func testSync(
    _ name: String,
    file: String = #file, line: UInt = #line,
    _ testFunction: @escaping @isolated(any) () -> Void
  ) {
    self.test(name, file: file, line: line) {
      await testFunction()
    }
  }
}

let ArrayCasts = TestSuite("ArrayCasts")

ArrayCasts.testSync("Funtion Conversion: covariant result") {
  typealias X = [() -> Derived]
  typealias Y = [() -> Base]

  let a: X = [
    { Derived(42) },
    { Derived(-1) },
    { Derived(37) }
  ]

  let b: Y = a
  expectEqual(b.count, 3)
  expectEqual(b[0]().k, 42)
  expectEqual(b[1]().k, -1)
  expectEqual(b[2]().k, 37)

  // Despite the warning saying that this cast always succeeds, it actually fails
  let c = a as? Y
  expectNil(c)

  let d = (a as Any) as? Y
  expectNil(d)
}

ArrayCasts.testSync("Funtion Conversion: contravariant argument") {
  typealias X = [(Base) -> Int]
  typealias Y = [(Derived) -> Int]

  let a: X = [
    { $0.k + 42 },
    { $0.k - 1 },
    { $0.k + 37 }
  ]

  let b: Y = a
  expectEqual(b.count, 3)
  let probe = Derived(10)
  expectEqual(b[0](probe), 52)
  expectEqual(b[1](probe), 9)
  expectEqual(b[2](probe), 47)

  // Despite the warning saying that this cast always succeeds, it actually fails
  let c = a as? Y
  expectNil(c)

  let d = (a as Any) as? Y
  expectNil(d)
}

#if _runtime(_ObjC)
ArrayCasts.testSync("Funtion Conversion: change calling convention 1") {
  typealias X = [(Base) -> Int]
  // This crashes compiler
  // See https://github.com/swiftlang/swift/issues/85082
  // typealias YA = [@convention(block) (Derived) -> Int]

  let a: X = [
    { $0.k + 42 },
    { $0.k - 1 },
    { $0.k + 37 }
  ]

  let b: [@convention(block) (Derived) -> Int] = a
  expectEqual(b.count, 3)
  let probe = Derived(10)
  expectEqual(b[0](probe), 52)
  expectEqual(b[1](probe), 9)
  expectEqual(b[2](probe), 47)

  // Despite the warning saying that this cast always succeeds, it actually fails
  let c = a as? [@convention(block) (Derived) -> Int]
  expectNil(c)

  let d = (a as Any) as? [@convention(block) (Derived) -> Int]
  expectNil(d)
}

ArrayCasts.testSync("Funtion Conversion: change calling convention 2") {
  // This crashes compiler
  // See https://github.com/swiftlang/swift/issues/85082
  // typealias X = [@convention(block) (Base) -> Int]
  typealias Y = [(Derived) -> Int]

  let a: [@convention(block) (Base) -> Int] = [
    { $0.k + 42 },
    { $0.k - 1 },
    { $0.k + 37 }
  ]

  let b: Y = a
  expectEqual(b.count, 3)
  let probe = Derived(10)
  expectEqual(b[0](probe), 52)
  expectEqual(b[1](probe), 9)
  expectEqual(b[2](probe), 47)

  // Despite the warning saying that this cast always succeeds, it actually fails
  let c = a as? Y
  expectNil(c)

  let d = (a as Any) as? Y
  expectNil(d)
}
#endif

ArrayCasts.testSync("Funtion Conversion: add isolation") {
  typealias X = [@Sendable () -> Int]
  typealias Y = [@MainActor () -> Int]

  print("0")
  let a: X = [
    { 42 },
    { -1 },
    { 37 }
  ]

  print("1")
  let b: Y = a
  print("2")
  expectEqual(b.count, 3)
  expectEqual(b[0](), 42)
  expectEqual(b[1](), -1)
  expectEqual(b[2](), 37)
  print("3")

  // Despite the warning saying that this cast always succeeds, it actually fails
  let c = a as? Y
  print("4")
  expectNil(c)
  print("5")

  let d = (a as Any) as? Y
  print("6")
  expectNil(d)
  print("7")
}

ArrayCasts.test("Funtion Conversion: as @isolated(any)").require(.stdlib_6_0).code {
  guard #available(SwiftStdlib 6.0, *) else { return }
  typealias X = [@MainActor () -> Int]
  typealias Y = [@isolated(any) () -> Int]

  let a: X = [
    { 42 },
    { -1 },
    { 37 }
  ]

  let b: Y = a
  expectEqual(b.count, 3)
  expectEqual(await b[0](), 42)
  expectEqual(await b[1](), -1)
  expectEqual(await b[2](), 37)

  // Despite the warning saying that this cast always succeeds, it actually fails
  let c = a as? Y
  expectNil(c)

  let d = (a as Any) as? Y
  expectNil(d)
}

ArrayCasts.testSync("Metatype Conversion") {
  typealias X = [Derived.Type]
  typealias Y = [Base.Type]

  let a: X = [
    Derived.self
  ]

  let b: Y = a
  expectEqual(b.count, 1)
  expectTrue(b[0] === Derived.self)

  let c = a as? Y
  expectTrue(c![0] === Derived.self)

  let d = (a as Any) as? Y
  expectTrue(d![0] === Derived.self)
}

let DictionaryCasts = TestSuite("DictionaryCasts")

DictionaryCasts.testSync("Funtion Conversion: covariant result") {
  typealias X = [String: () -> Derived]
  typealias Y = [AnyHashable: () -> Base]

  let a: X = [
    "abc": { Derived(42) },
    "xyz": { Derived(-1) },
    "ijk": { Derived(37) }
  ]

  let b: Y = a
  expectEqual(b.count, 3)
  expectEqual(b["abc"]?().k, 42)
  expectEqual(b["xyz"]?().k, -1)
  expectEqual(b["ijk"]?().k, 37)
}

await runAllTestsAsync()

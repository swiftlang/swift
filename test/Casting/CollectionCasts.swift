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
// RUN: %empty-directory(%t)
//
// RUN: %target-build-swift -Xfrontend -verify -Xfrontend -verify-additional-prefix -Xfrontend swift6- -swift-version 6 -g -Onone  -module-name a %s -o %t/a.swift6.Onone.out
// RUN: %target-codesign %t/a.swift6.Onone.out
// RUN: %target-run %t/a.swift6.Onone.out
//
// RUN: %target-build-swift -Xfrontend -verify -Xfrontend -verify-additional-prefix -Xfrontend swift6- -swift-version 6 -g -O  -module-name a %s -o %t/a.swift6.O.out
// RUN: %target-codesign %t/a.swift6.O.out
// RUN: %target-run %t/a.swift6.O.out
//
// RUN: %target-build-swift -Xfrontend -verify -swift-version 5 -g -Onone  -module-name a %s -o %t/a.swift5.Onone.out
// RUN: %target-codesign %t/a.swift5.Onone.out
// RUN: %target-run %t/a.swift5.Onone.out
//
// RUN: %target-build-swift -Xfrontend -verify -swift-version 5 -g -O  -module-name a %s -o %t/a.swift5.O.out
// RUN: %target-codesign %t/a.swift5.O.out
// RUN: %target-run %t/a.swift5.O.out
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

@preconcurrency func takeArray(_ x: [@MainActor @Sendable () -> String]) -> [@MainActor @Sendable () -> String] { x }
@preconcurrency func takeDictionary(_ x: [AnyHashable: @MainActor @Sendable () -> String]) -> [AnyHashable: @MainActor @Sendable () -> String] { x }

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
  // expected-warning@+1 {{conditional cast from 'X' (aka 'Array<() -> Derived>') to 'Y' (aka 'Array<() -> Base>') always succeeds}}
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
  // expected-warning@+1 {{conditional cast from 'X' (aka 'Array<(Base) -> Int>') to 'Y' (aka 'Array<(Derived) -> Int>') always succeeds}}
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
  // expected-warning@+1 {{conditional cast from 'X' (aka 'Array<(Base) -> Int>') to '[@convention(block) (Derived) -> Int]' always succeeds}}
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
  // expected-warning@+1 {{conditional cast from '[@convention(block) (Base) -> Int]' to 'Y' (aka 'Array<(Derived) -> Int>') always succeeds}}
  let c = a as? Y
  expectNil(c)

  let d = (a as Any) as? Y
  expectNil(d)
}
#endif

ArrayCasts.testSync("Funtion Conversion: add isolation") {
  typealias X = [@Sendable () -> Int]
  typealias Y = [@MainActor @Sendable () -> Int]

  let a: X = [
    { 42 },
    { -1 },
    { 37 }
  ]

  let b: Y = a
  expectEqual(b.count, 3)
  expectEqual(b[0](), 42)
  expectEqual(b[1](), -1)
  expectEqual(b[2](), 37)

  // Despite the warning saying that this cast always succeeds, it actually fails
  // expected-warning@+1 {{conditional cast from 'X' (aka 'Array<@Sendable () -> Int>') to 'Y' (aka 'Array<@MainActor @Sendable () -> Int>') always succeeds}}
  let c = a as? Y
  expectNil(c)

  let d = (a as Any) as? Y
  expectNil(d)
}

ArrayCasts.test("Funtion Conversion: as @isolated(any)").require(.stdlib_6_0).code {
  guard #available(SwiftStdlib 6.0, *) else { return }
  typealias X = [@MainActor @Sendable () -> Int]
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
  // expected-warning@+1 {{conditional cast from 'X' (aka 'Array<@MainActor @Sendable () -> Int>') to 'Y' (aka 'Array<@isolated(any) () -> Int>') always succeeds}}
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

  // Indeed succeeds
  // expected-warning@+1 {{conditional cast from 'X' (aka 'Array<Derived.Type>') to 'Y' (aka 'Array<Base.Type>') always succeeds}}
  let c = a as? Y
  expectTrue(c![0] === Derived.self)

  let d = (a as Any) as? Y
  expectTrue(d![0] === Derived.self)
}

ArrayCasts.testSync("Erasure 1") {
  typealias X = [Base]
  typealias Y = [AnyObject]

  let a: X = [
    Base(42),
    Base(-1),
    Base(37)
  ]

  let b: Y = a
  expectEqual(b.count, 3)
  expectTrue(b[0] === a[0])
  expectTrue(b[1] === a[1])
  expectTrue(b[2] === a[2])

  // Indeed succeeds
  // expected-warning@+1 {{conditional cast from 'X' (aka 'Array<Base>') to 'Y' (aka 'Array<AnyObject>') always succeeds}}
  let c = a as? Y
  expectEqual(c!.count, 3)
  expectTrue(c![0] === a[0])
  expectTrue(c![1] === a[1])
  expectTrue(c![2] === a[2])

  let d = (a as Any) as? Y
  expectEqual(d!.count, 3)
  expectTrue(d![0] === a[0])
  expectTrue(d![1] === a[1])
  expectTrue(d![2] === a[2])
}

ArrayCasts.testSync("Erasure 2") {
  typealias X = [String]
  typealias Y = [any Hashable]

  let a: X = [
    "abc",
    "xyz",
    "ijk"
  ]

  let b: Y = a
  expectEqual(b.count, 3)
  expectTrue(b[0].hashValue == a[0].hashValue)
  expectTrue(b[1].hashValue == a[1].hashValue)
  expectTrue(b[2].hashValue == a[2].hashValue)

  // Indeed succeeds
  // expected-warning@+1 {{conditional cast from 'X' (aka 'Array<String>') to 'Y' (aka 'Array<any Hashable>') always succeeds}}
  let c = a as? Y
  expectEqual(c!.count, 3)
  expectTrue(c![0].hashValue == a[0].hashValue)
  expectTrue(c![1].hashValue == a[1].hashValue)
  expectTrue(c![2].hashValue == a[2].hashValue)

  let d = (a as Any) as? Y
  expectEqual(d!.count, 3)
  expectTrue(d![0].hashValue == a[0].hashValue)
  expectTrue(d![1].hashValue == a[1].hashValue)
  expectTrue(d![2].hashValue == a[2].hashValue)
}

ArrayCasts.testSync("AnyHashable erasure") {
  typealias X = [String]
  typealias Y = [AnyHashable]

  let a: X = [
    "abc",
    "xyz",
    "ijk"
  ]

  let b: Y = a
  expectEqual(b.count, 3)
  expectTrue(b[0].hashValue == a[0].hashValue)
  expectTrue(b[1].hashValue == a[1].hashValue)
  expectTrue(b[2].hashValue == a[2].hashValue)

  // Indeed succeeds
  // expected-warning@+1 {{conditional cast from 'X' (aka 'Array<String>') to 'Y' (aka 'Array<AnyHashable>') always succeeds}}
  let c = a as? Y
  expectEqual(c!.count, 3)
  expectTrue(c![0].hashValue == a[0].hashValue)
  expectTrue(c![1].hashValue == a[1].hashValue)
  expectTrue(c![2].hashValue == a[2].hashValue)

  let d = (a as Any) as? Y
  expectEqual(d!.count, 3)
  expectTrue(d![0].hashValue == a[0].hashValue)
  expectTrue(d![1].hashValue == a[1].hashValue)
  expectTrue(d![2].hashValue == a[2].hashValue)
}

ArrayCasts.testSync("Bridge To ObjC 1") {
  typealias X = [String]
  typealias Y = [AnyObject]

  let a: X = [
    "abc",
    "xyz",
    "ijk"
  ]

  let b = a as Y
  expectEqual(b.count, 3)
  expectTrue((b[0] as! String) == a[0])
  expectTrue((b[1] as! String) == a[1])
  expectTrue((b[2] as! String) == a[2])

  // Indeed succeeds
  // expected-warning@+1 {{conditional cast from 'X' (aka 'Array<String>') to 'Y' (aka 'Array<AnyObject>') always succeeds}}
  let c = a as? Y
  expectEqual(c!.count, 3)
  expectTrue((c![0] as! String) == a[0])
  expectTrue((c![1] as! String) == a[1])
  expectTrue((c![2] as! String) == a[2])

  let d = (a as Any) as? Y
  expectEqual(d!.count, 3)
  expectTrue((d![0] as! String) == a[0])
  expectTrue((d![1] as! String) == a[1])
  expectTrue((d![2] as! String) == a[2])
}

#if _runtime(_ObjC)
ArrayCasts.testSync("Bridge To ObjC 2") {
  typealias X = [String]
  typealias Y = [NSString]

  let a: X = [
    "abc",
    "xyz",
    "ijk"
  ]

  let b = a as Y
  expectEqual(b.count, 3)
  expectTrue(b[0] as String == a[0])
  expectTrue(b[1] as String == a[1])
  expectTrue(b[2] as String == a[2])

  // Indeed succeeds
  // expected-warning@+1 {{conditional cast from 'X' (aka 'Array<String>') to 'Y' (aka 'Array<NSString>') always succeeds}}
  let c = a as? Y
  expectEqual(c!.count, 3)
  expectTrue((c![0] as String) == a[0])
  expectTrue((c![1] as String) == a[1])
  expectTrue((c![2] as String) == a[2])

  let d = (a as Any) as? Y
  expectEqual(d!.count, 3)
  expectTrue((d![0] as String) == a[0])
  expectTrue((d![1] as String) == a[1])
  expectTrue((d![2] as String) == a[2])
}

ArrayCasts.testSync("Bridge To ObjC 3") {
  typealias X = [String]
  typealias Y = [CFString]

  let a: X = [
    "abc",
    "xyz",
    "ijk"
  ]

  let b = a as Y
  expectEqual(b.count, 3)
  expectTrue(b[0] as String == a[0])
  expectTrue(b[1] as String == a[1])
  expectTrue(b[2] as String == a[2])

  // Indeed succeeds
  // expected-warning@+1 {{conditional cast from 'X' (aka 'Array<String>') to 'Y' (aka 'Array<CFString>') always succeeds}}
  let c = a as? Y
  expectEqual(c!.count, 3)
  expectTrue((c![0] as String) == a[0])
  expectTrue((c![1] as String) == a[1])
  expectTrue((c![2] as String) == a[2])

  let d = (a as Any) as? Y
  expectEqual(d!.count, 3)
  expectTrue((d![0] as String) == a[0])
  expectTrue((d![1] as String) == a[1])
  expectTrue((d![2] as String) == a[2])
}
#endif

ArrayCasts.testSync("Metatype to AnyObject 1") {
  typealias X = [String.Type]
  typealias Y = [AnyObject]

  let a: X = [
    String.self
  ]

  let b = a as Y
  expectEqual(b.count, 1)
  expectTrue((b[0] as! Any.Type) == (a[0] as Any.Type))

  // Indeed succeeds
  // expected-warning@+1 {{conditional cast from 'X' (aka 'Array<String.Type>') to 'Y' (aka 'Array<AnyObject>') always succeeds}}
  let c = a as? Y
  expectEqual(c!.count, 1)
  expectTrue((c![0] as! Any.Type) == (a[0] as Any.Type))

  let d = (a as Any) as? Y
  expectEqual(d!.count, 1)
  expectTrue((d![0] as! Any.Type) == (a[0] as Any.Type))
}

ArrayCasts.testSync("Metatype to AnyObject 2") {
  typealias X = [(any Hashable).Type]
  typealias Y = [AnyObject]

  let a: X = [
    (any Hashable).self
  ]

  let b = a as Y
  expectEqual(b.count, 1)
  expectTrue((b[0] as! Any.Type) == (a[0] as Any.Type))

  // Indeed succeeds
  // expected-warning@+1 {{conditional cast from 'X' (aka 'Array<(any Hashable).Type>') to 'Y' (aka 'Array<AnyObject>') always succeeds}}
  let c = a as? Y
  expectEqual(c!.count, 1)
  expectTrue((c![0] as! Any.Type) == (a[0] as Any.Type))

  let d = (a as Any) as? Y
  expectEqual(d!.count, 1)
  expectTrue((d![0] as! Any.Type) == (a[0] as Any.Type))
}

ArrayCasts.testSync("Metatype to AnyObject 3") {
  typealias X = [any Hashable.Type]
  typealias Y = [AnyObject]

  let a: X = [
    Int.self,
    String.self,
    AnyHashable.self
  ]

  let b = a as Y
  expectEqual(b.count, 3)
  expectTrue((b[0] as! Any.Type) == a[0])
  expectTrue((b[1] as! Any.Type) == a[1])
  expectTrue((b[2] as! Any.Type) == a[2])

  // Indeed succeeds
  // expected-warning@+1 {{conditional cast from 'X' (aka 'Array<any Hashable.Type>') to 'Y' (aka 'Array<AnyObject>') always succeeds}}
  let c = a as? Y
  expectEqual(c!.count, 3)
  expectTrue((c![0] as! Any.Type) == a[0])
  expectTrue((c![1] as! Any.Type) == a[1])
  expectTrue((c![2] as! Any.Type) == a[2])

  let d = (a as Any) as? Y
  expectEqual(d!.count, 3)
  expectTrue((d![0] as! Any.Type) == a[0])
  expectTrue((d![1] as! Any.Type) == a[1])
  expectTrue((d![2] as! Any.Type) == a[2])
}

ArrayCasts.testSync("Nested") {
  typealias X = [[String]]
  typealias Y = [[AnyObject]]

  let a: X = [
    ["xyz", "abc"],
    ["ijk"],
    []
  ]

  let b = a as Y
  expectEqual(b.count, 3)
  expectEqual(b[0].count, 2)
  expectEqual(b[1].count, 1)
  expectEqual(b[2].count, 0)
  expectTrue((b[0][0] as! String) == "xyz")
  expectTrue((b[0][1] as! String) == "abc")
  expectTrue((b[1][0] as! String) == "ijk")

  // Indeed succeeds
  // expected-warning@+1 {{conditional cast from 'X' (aka 'Array<Array<String>>') to 'Y' (aka 'Array<Array<AnyObject>>') always succeeds}}
  let c = a as? Y
  expectEqual(c!.count, 3)
  expectEqual(c![0].count, 2)
  expectEqual(c![1].count, 1)
  expectEqual(c![2].count, 0)
  expectTrue((c![0][0] as! String) == "xyz")
  expectTrue((c![0][1] as! String) == "abc")
  expectTrue((c![1][0] as! String) == "ijk")

  let d = (a as Any) as? Y
  expectEqual(d!.count, 3)
  expectEqual(d![0].count, 2)
  expectEqual(d![1].count, 1)
  expectEqual(d![2].count, 0)
  expectTrue((d![0][0] as! String) == "xyz")
  expectTrue((d![0][1] as! String) == "abc")
  expectTrue((d![1][0] as! String) == "ijk")
}

ArrayCasts.testSync("Covariant") {

  typealias X = [([any StringProtocol]) -> [String]]
  typealias Y = [([String]) -> [any StringProtocol]]

  let a: X = [
    { $0 as! [String] },
    { _ in ["abc"] },
    { ($0 + $0) as! [String] }
  ]

  let b = a as Y
  expectEqual(b.count, 3)
  expectEqual(b[0](["xyz", "ijk"]) as! [String], ["xyz", "ijk"])
  expectEqual(b[1](["xyz", "ijk"]) as! [String], ["abc"])
  expectEqual(b[2](["xyz", "ijk"]) as! [String], ["xyz", "ijk", "xyz", "ijk"])

  // Despite the warning saying that this cast always succeeds, it actually fails
  // expected-warning@+1 {{conditional cast from 'X' (aka 'Array<(Array<any StringProtocol>) -> Array<String>>') to 'Y' (aka 'Array<(Array<String>) -> Array<any StringProtocol>>') always succeeds}}
  let c = a as? Y
  expectNil(c)

  let d = (a as Any) as? Y
  expectNil(d)
}

// Currently failing
// See https://forums.swift.org/t/casting-closure-collections/82100/10
if false {
ArrayCasts.testSync("Covariant nested") {

  typealias X = [([() -> any StringProtocol]) -> [String]]
  typealias Y = [([() -> String]) -> [any StringProtocol]]

  let a: X = [
    { x in x.map { $0() as! String } },
    { _ in ["abc"] },
    { x in (x + x).map { $0() as! String } }
  ]

  let b = a as Y
  expectEqual(b.count, 3)
  let probe: [() -> String] = [{ "xyz" }, { "ijk" } ]
  expectEqual(b[0](probe) as! [String], ["xyz", "ijk"])
  expectEqual(b[1](probe) as! [String], ["abc"])
  expectEqual(b[2](probe) as! [String], ["xyz", "ijk", "xyz", "ijk"])

  // Despite the warning saying that this cast always succeeds, it actually fails
  // expected-warning@+1 {{conditional cast from 'X' (aka 'Array<(Array<() -> any StringProtocol>) -> Array<String>>') to 'Y' (aka 'Array<(Array<() -> String>) -> Array<any StringProtocol>>') always succeeds}}
  let c = a as? Y
  expectNil(c)

  let d = (a as Any) as? Y
  expectNil(d)
}
}

ArrayCasts.testSync("Preconcurrency") {

  typealias X = [() -> String]
  typealias Y = [@MainActor @Sendable () -> String]

  let a: X = [
    { "abc" },
    { "xyz" },
    { "ijk" }
  ]

  // expected-swift6-warning@+1 {{converting non-Sendable function value to '@MainActor @Sendable () -> String' may introduce data races}}
  let b: Y = takeArray(a)
  expectEqual(b.count, 3)
  expectEqual(b[0](), "abc")
  expectEqual(b[1](), "xyz")
  expectEqual(b[2](), "ijk")

  // Despite the warning saying that this cast always succeeds, it actually fails
  // expected-warning@+1 {{conditional cast from 'X' (aka 'Array<() -> String>') to 'Y' (aka 'Array<@MainActor @Sendable () -> String>') always succeeds}}
  let c = a as? Y
  expectNil(c)

  let d = (a as Any) as? Y
  expectNil(d)
}

let DictionaryCasts = TestSuite("DictionaryCasts")

DictionaryCasts.testSync("Funtion Conversion: covariant result") {
  typealias X = [String: () -> Derived]
  typealias Y = [String: () -> Base]

  let a: X = [
    "a": { Derived(42) },
    "b": { Derived(-1) },
    "c": { Derived(37) }
  ]

  let b: Y = a
  expectEqual(b.count, 3)
  expectEqual(b["a"]!().k, 42)
  expectEqual(b["b"]!().k, -1)
  expectEqual(b["c"]!().k, 37)

  // Despite the warning saying that this cast always succeeds, it actually fails
  // expected-warning@+1 {{conditional cast from 'X' (aka 'Dictionary<String, () -> Derived>') to 'Y' (aka 'Dictionary<String, () -> Base>') always succeeds}}
  let c = a as? Y
  expectNil(c)

  let d = (a as Any) as? Y
  expectNil(d)
}

DictionaryCasts.testSync("Funtion Conversion: contravariant argument") {
  typealias X = [String: (Base) -> Int]
  typealias Y = [AnyHashable: (Derived) -> Int]

  let a: X = [
    "a": { $0.k + 42 },
    "b": { $0.k - 1 },
    "c": { $0.k + 37 }
  ]

  let b: Y = a
  expectEqual(b.count, 3)
  let probe = Derived(10)
  expectEqual(b["a"]!(probe), 52)
  expectEqual(b["b"]!(probe), 9)
  expectEqual(b["c"]!(probe), 47)

  // Despite the warning saying that this cast always succeeds, it actually fails
  // expected-warning@+1 {{conditional cast from 'X' (aka 'Dictionary<String, (Base) -> Int>') to 'Y' (aka 'Dictionary<AnyHashable, (Derived) -> Int>') always succeeds}}
  let c = a as? Y
  expectNil(c)

  let d = (a as Any) as? Y
  expectNil(d)
}

#if _runtime(_ObjC)
DictionaryCasts.testSync("Funtion Conversion: change calling convention 1") {
  typealias X = [String: (Base) -> Int]
  // This crashes compiler
  // See https://github.com/swiftlang/swift/issues/85082
  // typealias YA = [String: @convention(block) (Derived) -> Int]

  let a: X = [
    "a": { $0.k + 42 },
    "b": { $0.k - 1 },
    "c": { $0.k + 37 }
  ]

  let b: [String: @convention(block) (Derived) -> Int] = a
  expectEqual(b.count, 3)
  let probe = Derived(10)
  expectEqual(b["a"]!(probe), 52)
  expectEqual(b["b"]!(probe), 9)
  expectEqual(b["c"]!(probe), 47)

  // Despite the warning saying that this cast always succeeds, it actually fails
  // expected-warning@+1 {{conditional cast from 'X' (aka 'Dictionary<String, (Base) -> Int>') to '[String : @convention(block) (Derived) -> Int]' always succeeds}}
  let c = a as? [String: @convention(block) (Derived) -> Int]
  expectNil(c)

  let d = (a as Any) as? [String: @convention(block) (Derived) -> Int]
  expectNil(d)
}

DictionaryCasts.testSync("Funtion Conversion: change calling convention 2") {
  // This crashes compiler
  // See https://github.com/swiftlang/swift/issues/85082
  // typealias X = [@convention(block) (Base) -> Int]
  typealias Y = [String: (Derived) -> Int]

  let a: [String: @convention(block) (Base) -> Int] = [
    "a": { $0.k + 42 },
    "b": { $0.k - 1 },
    "c": { $0.k + 37 }
  ]

  let b: Y = a
  expectEqual(b.count, 3)
  let probe = Derived(10)
  expectEqual(b["a"]!(probe), 52)
  expectEqual(b["b"]!(probe), 9)
  expectEqual(b["c"]!(probe), 47)

  // Despite the warning saying that this cast always succeeds, it actually fails
  // expected-warning@+1 {{conditional cast from '[String : @convention(block) (Base) -> Int]' to 'Y' (aka 'Dictionary<String, (Derived) -> Int>') always succeeds}}
  let c = a as? Y
  expectNil(c)

  let d = (a as Any) as? Y
  expectNil(d)
}
#endif

DictionaryCasts.testSync("Funtion Conversion: add isolation") {
  typealias X = [String: @Sendable () -> Int]
  typealias Y = [AnyHashable: @MainActor @Sendable () -> Int]

  let a: X = [
    "a": { 42 },
    "b": { -1 },
    "c": { 37 }
  ]

  let b: Y = a
  expectEqual(b.count, 3)
  expectEqual(b["a"]!(), 42)
  expectEqual(b["b"]!(), -1)
  expectEqual(b["c"]!(), 37)

  // Despite the warning saying that this cast always succeeds, it actually fails
  // expected-warning@+1 {{conditional cast from 'X' (aka 'Dictionary<String, @Sendable () -> Int>') to 'Y' (aka 'Dictionary<AnyHashable, @MainActor @Sendable () -> Int>') always succeeds}}
  let c = a as? Y
  expectNil(c)

  let d = (a as Any) as? Y
  expectNil(d)
}

DictionaryCasts.test("Funtion Conversion: as @isolated(any)").require(.stdlib_6_0).code {
  guard #available(SwiftStdlib 6.0, *) else { return }
  typealias X = [String: @MainActor @Sendable () -> Int]
  typealias Y = [String: @isolated(any) () -> Int]

  let a: X = [
    "a": { 42 },
    "b": { -1 },
    "c": { 37 }
  ]

  let b: Y = a
  expectEqual(b.count, 3)
  expectEqual(await b["a"]!(), 42)
  expectEqual(await b["b"]!(), -1)
  expectEqual(await b["c"]!(), 37)

  // Despite the warning saying that this cast always succeeds, it actually fails
  // expected-warning@+1 {{conditional cast from 'X' (aka 'Dictionary<String, @MainActor @Sendable () -> Int>') to 'Y' (aka 'Dictionary<String, @isolated(any) () -> Int>') always succeeds}}
  let c = a as? Y
  expectNil(c)

  let d = (a as Any) as? Y
  expectNil(d)
}

DictionaryCasts.testSync("Metatype Conversion") {
  typealias X = [String: Derived.Type]
  typealias Y = [AnyHashable: Base.Type]

  let a: X = [
    "a": Derived.self
  ]

  let b: Y = a
  expectEqual(b.count, 1)
  expectTrue(b["a"]! === Derived.self)

  // Indeed succeeds
  // expected-warning@+1 {{conditional cast from 'X' (aka 'Dictionary<String, Derived.Type>') to 'Y' (aka 'Dictionary<AnyHashable, Base.Type>') always succeeds}}
  let c = a as? Y
  expectTrue(c!["a"]! === Derived.self)

  let d = (a as Any) as? Y
  expectTrue(d!["a"]! === Derived.self)
}

DictionaryCasts.testSync("Erasure 1") {
  typealias X = [String: Base]
  typealias Y = [String: AnyObject]

  let a: X = [
    "a": Base(42),
    "b": Base(-1),
    "c": Base(37)
  ]

  let b: Y = a
  expectEqual(b.count, 3)
  expectTrue(b["a"]! === a["a"]!)
  expectTrue(b["b"]! === a["b"]!)
  expectTrue(b["c"]! === a["c"]!)

  // Indeed succeeds
  // expected-warning@+1 {{conditional cast from 'X' (aka 'Dictionary<String, Base>') to 'Y' (aka 'Dictionary<String, AnyObject>') always succeeds}}
  let c = a as? Y
  expectEqual(c!.count, 3)
  expectTrue(c!["a"]! === a["a"]!)
  expectTrue(c!["b"]! === a["b"]!)
  expectTrue(c!["c"]! === a["c"]!)

  let d = (a as Any) as? Y
  expectEqual(d!.count, 3)
  expectTrue(d!["a"]! === a["a"]!)
  expectTrue(d!["b"]! === a["b"]!)
  expectTrue(d!["c"]! === a["c"]!)
}

DictionaryCasts.testSync("Erasure 2") {
  typealias X = [String: String]
  typealias Y = [AnyHashable: any Hashable]

  let a: X = [
    "a": "abc",
    "b": "xyz",
    "c": "ijk"
  ]

  let b: Y = a
  expectEqual(b.count, 3)
  expectTrue(b["a"]!.hashValue == a["a"]!.hashValue)
  expectTrue(b["b"]!.hashValue == a["b"]!.hashValue)
  expectTrue(b["c"]!.hashValue == a["c"]!.hashValue)

  // Indeed succeeds
  // expected-warning@+1 {{conditional cast from 'X' (aka 'Dictionary<String, String>') to 'Y' (aka 'Dictionary<AnyHashable, any Hashable>') always succeeds}}
  let c = a as? Y
  expectEqual(c!.count, 3)
  expectTrue(c!["a"]!.hashValue == a["a"]!.hashValue)
  expectTrue(c!["b"]!.hashValue == a["b"]!.hashValue)
  expectTrue(c!["c"]!.hashValue == a["c"]!.hashValue)

  let d = (a as Any) as? Y
  expectEqual(d!.count, 3)
  expectTrue(d!["a"]!.hashValue == a["a"]!.hashValue)
  expectTrue(d!["b"]!.hashValue == a["b"]!.hashValue)
  expectTrue(d!["c"]!.hashValue == a["c"]!.hashValue)
}

DictionaryCasts.testSync("AnyHashable erasure") {
  typealias X = [String: String]
  typealias Y = [String: AnyHashable]

  let a: X = [
    "a": "abc",
    "b": "xyz",
    "c": "ijk"
  ]

  let b: Y = a
  expectEqual(b.count, 3)
  expectTrue(b["a"]!.hashValue == a["a"]!.hashValue)
  expectTrue(b["b"]!.hashValue == a["b"]!.hashValue)
  expectTrue(b["c"]!.hashValue == a["c"]!.hashValue)

  // Indeed succeeds
  // expected-warning@+1 {{conditional cast from 'X' (aka 'Dictionary<String, String>') to 'Y' (aka 'Dictionary<String, AnyHashable>') always succeeds}}
  let c = a as? Y
  expectEqual(c!.count, 3)
  expectTrue(c!["a"]!.hashValue == a["a"]!.hashValue)
  expectTrue(c!["b"]!.hashValue == a["b"]!.hashValue)
  expectTrue(c!["c"]!.hashValue == a["c"]!.hashValue)

  let d = (a as Any) as? Y
  expectEqual(d!.count, 3)
  expectTrue(d!["a"]!.hashValue == a["a"]!.hashValue)
  expectTrue(d!["b"]!.hashValue == a["b"]!.hashValue)
  expectTrue(d!["c"]!.hashValue == a["c"]!.hashValue)
}

DictionaryCasts.testSync("Bridge To ObjC 1") {
  typealias X = [String: String]
  typealias Y = [AnyHashable: AnyObject]

  let a: X = [
    "a": "abc",
    "b": "xyz",
    "c": "ijk"
  ]

  let b = a as Y
  expectEqual(b.count, 3)
  expectTrue((b["a"]! as! String) == a["a"]!)
  expectTrue((b["b"]! as! String) == a["b"]!)
  expectTrue((b["c"]! as! String) == a["c"]!)

  // Indeed succeeds
  // expected-warning@+1 {{conditional cast from 'X' (aka 'Dictionary<String, String>') to 'Y' (aka 'Dictionary<AnyHashable, AnyObject>') always succeeds}}
  let c = a as? Y
  expectEqual(c!.count, 3)
  expectTrue((c!["a"]! as! String) == a["a"]!)
  expectTrue((c!["b"]! as! String) == a["b"]!)
  expectTrue((c!["c"]! as! String) == a["c"]!)

  let d = (a as Any) as? Y
  expectEqual(d!.count, 3)
  expectTrue((d!["a"]! as! String) == a["a"]!)
  expectTrue((d!["b"]! as! String) == a["b"]!)
  expectTrue((d!["c"]! as! String) == a["c"]!)
}

#if _runtime(_ObjC)
DictionaryCasts.testSync("Bridge To ObjC 2") {
  typealias X = [String: String]
  typealias Y = [String: NSString]

  let a: X = [
    "a": "abc",
    "b": "xyz",
    "c": "ijk"
  ]

  let b = a as Y
  expectEqual(b.count, 3)
  expectTrue(b["a"]! as String == a["a"]!)
  expectTrue(b["b"]! as String == a["b"]!)
  expectTrue(b["c"]! as String == a["c"]!)

  // Indeed succeeds
  // expected-warning@+1 {{conditional cast from 'X' (aka 'Dictionary<String, String>') to 'Y' (aka 'Dictionary<String, NSString>') always succeeds}}
  let c = a as? Y
  expectEqual(c!.count, 3)
  expectTrue((c!["a"]! as String) == a["a"]!)
  expectTrue((c!["b"]! as String) == a["b"]!)
  expectTrue((c!["c"]! as String) == a["c"]!)

  let d = (a as Any) as? Y
  expectEqual(d!.count, 3)
  expectTrue((d!["a"]! as String) == a["a"]!)
  expectTrue((d!["b"]! as String) == a["b"]!)
  expectTrue((d!["c"]! as String) == a["c"]!)
}

DictionaryCasts.testSync("Bridge To ObjC 3") {
  typealias X = [String: String]
  typealias Y = [AnyHashable: CFString]

  let a: X = [
    "a": "abc",
    "b": "xyz",
    "c": "ijk"
  ]

  let b = a as Y
  expectEqual(b.count, 3)
  expectTrue(b["a"]! as String == a["a"]!)
  expectTrue(b["b"]! as String == a["b"]!)
  expectTrue(b["c"]! as String == a["c"]!)

  // Indeed succeeds
  // expected-warning@+1 {{conditional cast from 'X' (aka 'Dictionary<String, String>') to 'Y' (aka 'Dictionary<AnyHashable, CFString>') always succeeds}}
  let c = a as? Y
  expectEqual(c!.count, 3)
  expectTrue((c!["a"]! as String) == a["a"]!)
  expectTrue((c!["b"]! as String) == a["b"]!)
  expectTrue((c!["c"]! as String) == a["c"]!)

  let d = (a as Any) as? Y
  expectEqual(d!.count, 3)
  expectTrue((d!["a"]! as String) == a["a"]!)
  expectTrue((d!["b"]! as String) == a["b"]!)
  expectTrue((d!["c"]! as String) == a["c"]!)
}
#endif

DictionaryCasts.testSync("Metatype to AnyObject 1") {
  typealias X = [String: String.Type]
  typealias Y = [String: AnyObject]

  let a: X = [
    "a": String.self
  ]

  let b = a as Y
  expectEqual(b.count, 1)
  expectTrue((b["a"]! as! Any.Type) == (a["a"]! as Any.Type))

  // Indeed succeeds
  // expected-warning@+1 {{conditional cast from 'X' (aka 'Dictionary<String, String.Type>') to 'Y' (aka 'Dictionary<String, AnyObject>') always succeeds}}
  let c = a as? Y
  expectEqual(c!.count, 1)
  expectTrue((c!["a"]! as! Any.Type) == (a["a"]! as Any.Type))

  let d = (a as Any) as? Y
  expectEqual(d!.count, 1)
  expectTrue((d!["a"]! as! Any.Type) == (a["a"]! as Any.Type))
}

DictionaryCasts.testSync("Metatype to AnyObject 2") {
  typealias X = [String: (any Hashable).Type]
  typealias Y = [AnyHashable: AnyObject]

  let a: X = [
    "a": (any Hashable).self
  ]

  let b = a as Y
  expectEqual(b.count, 1)
  expectTrue((b["a"]! as! Any.Type) == (a["a"]! as Any.Type))

  // Indeed succeeds
  // expected-warning@+1 {{conditional cast from 'X' (aka 'Dictionary<String, (any Hashable).Type>') to 'Y' (aka 'Dictionary<AnyHashable, AnyObject>') always succeeds}}
  let c = a as? Y
  expectEqual(c!.count, 1)
  expectTrue((c!["a"]! as! Any.Type) == (a["a"]! as Any.Type))

  let d = (a as Any) as? Y
  expectEqual(d!.count, 1)
  expectTrue((d!["a"]! as! Any.Type) == (a["a"]! as Any.Type))
}

DictionaryCasts.testSync("Metatype to AnyObject 3") {
  typealias X = [String: any Hashable.Type]
  typealias Y = [String: AnyObject]

  let a: X = [
    "a": Int.self,
    "b": String.self,
    "c": AnyHashable.self
  ]

  let b = a as Y
  expectEqual(b.count, 3)
  expectTrue((b["a"]! as! Any.Type) == a["a"]!)
  expectTrue((b["b"]! as! Any.Type) == a["b"]!)
  expectTrue((b["c"]! as! Any.Type) == a["c"]!)

  // Indeed succeeds
  // expected-warning@+1 {{conditional cast from 'X' (aka 'Dictionary<String, any Hashable.Type>') to 'Y' (aka 'Dictionary<String, AnyObject>') always succeeds}}
  let c = a as? Y
  expectEqual(c!.count, 3)
  expectTrue((c!["a"]! as! Any.Type) == a["a"]!)
  expectTrue((c!["b"]! as! Any.Type) == a["b"]!)
  expectTrue((c!["c"]! as! Any.Type) == a["c"]!)

  let d = (a as Any) as? Y
  expectEqual(d!.count, 3)
  expectTrue((d!["a"]! as! Any.Type) == a["a"]!)
  expectTrue((d!["b"]! as! Any.Type) == a["b"]!)
  expectTrue((d!["c"]! as! Any.Type) == a["c"]!)
}

DictionaryCasts.testSync("Nested") {
  typealias X = [String: [String: String]]
  typealias Y = [AnyHashable: [AnyHashable: AnyObject]]

  let a: X = [
    "a": ["a": "xyz", "b": "abc"],
    "b": ["a": "ijk"],
    "c": [:]
  ]

  let b = a as Y
  expectEqual(b.count, 3)
  expectEqual(b["a"]!.count, 2)
  expectEqual(b["b"]!.count, 1)
  expectEqual(b["c"]!.count, 0)
  expectTrue((b["a"]!["a"]! as! String) == "xyz")
  expectTrue((b["a"]!["b"]! as! String) == "abc")
  expectTrue((b["b"]!["a"]! as! String) == "ijk")

  // Indeed succeeds
  // expected-warning@+1 {{conditional cast from 'X' (aka 'Dictionary<String, Dictionary<String, String>>') to 'Y' (aka 'Dictionary<AnyHashable, Dictionary<AnyHashable, AnyObject>>') always succeeds}}
  let c = a as? Y
  expectEqual(c!.count, 3)
  expectEqual(c!["a"]!.count, 2)
  expectEqual(c!["b"]!.count, 1)
  expectEqual(c!["c"]!.count, 0)
  expectTrue((c!["a"]!["a"]! as! String) == "xyz")
  expectTrue((c!["a"]!["b"]! as! String) == "abc")
  expectTrue((c!["b"]!["a"]! as! String) == "ijk")

  let d = (a as Any) as? Y
  expectEqual(d!.count, 3)
  expectEqual(d!["a"]!.count, 2)
  expectEqual(d!["b"]!.count, 1)
  expectEqual(d!["c"]!.count, 0)
  expectTrue((d!["a"]!["a"]! as! String) == "xyz")
  expectTrue((d!["a"]!["b"]! as! String) == "abc")
  expectTrue((d!["b"]!["a"]! as! String) == "ijk")
}

DictionaryCasts.testSync("Covariant") {

  typealias X = [String: ([String: any StringProtocol]) -> [String: String]]
  typealias Y = [String: ([String: String]) -> [String: any StringProtocol]]

  let a: X = [
    "a": { $0 as! [String: String] },
    "b": { _ in ["a": "abc"] },
    "c": {
      var result = $0 as! [String: String]
      for (key, value) in $0 {
        let vs = value as! String
        result[key + key] = vs + vs
      }
      return result
    }
  ]

  let b = a as Y
  expectEqual(b.count, 3)
  let probe: [String: String] = ["a": "xyz", "b": "ijk"]
  expectEqual(b["a"]!(probe) as! [String: String], probe)
  expectEqual(b["b"]!(probe) as! [String: String], ["a": "abc"])
  expectEqual(b["c"]!(probe) as! [String: String], ["a": "xyz", "b": "ijk", "aa": "xyzxyz", "bb": "ijkijk"])

  // Despite the warning saying that this cast always succeeds, it actually fails
  // expected-warning@+1 {{conditional cast from 'X' (aka 'Dictionary<String, (Dictionary<String, any StringProtocol>) -> Dictionary<String, String>>') to 'Y' (aka 'Dictionary<String, (Dictionary<String, String>) -> Dictionary<String, any StringProtocol>>') always succeeds}}
  let c = a as? Y
  expectNil(c)

  let d = (a as Any) as? Y
  expectNil(d)
}

// Currently failing
// See https://forums.swift.org/t/casting-closure-collections/82100/10
if false {
DictionaryCasts.testSync("Covariant nested") {

  typealias X = [String: ([String: () -> any StringProtocol]) -> [String: String]]
  typealias Y = [String: ([String: () -> String]) -> [String: any StringProtocol]]

  let a: X = [
    "a": { (x: [String: () -> any StringProtocol]) -> [String: String] in
      x.mapValues { $0() as! String }
    },
    "b": { _ in  ["a": "abc"] },
    "c": { (x: [String: () -> any StringProtocol]) -> [String: String] in
      let tmp: [String: String] = x.mapValues { f in f() as! String }
      var result = tmp
      for (key, value) in tmp {
        result[key + key] = value + value
      }
      return result
    }
  ]

  let b = a as Y
  expectEqual(b.count, 3)
  let probe: [String: () -> String] = ["a": { "xyz" }, "b": { "ijk" } ]
  expectEqual(b["a"]!(probe) as! [String: String], ["a": "xyz", "b": "ijk"])
  expectEqual(b["b"]!(probe) as! [String: String], ["a": "abc"])
  expectEqual(b["c"]!(probe) as! [String: String], ["a": "xyz", "b": "ijk", "aa": "xyzxyz", "bb": "ijkijk"])

  // Despite the warning saying that this cast always succeeds, it actually fails
  // expected-warning@+1 {{conditional cast from 'X' (aka 'Dictionary<String, (Dictionary<String, () -> any StringProtocol>) -> Dictionary<String, String>>') to 'Y' (aka 'Dictionary<String, (Dictionary<String, () -> String>) -> Dictionary<String, any StringProtocol>>') always succeeds}}
  let c = a as? Y
  expectNil(c)

  let d = (a as Any) as? Y
  expectNil(d)
}
}

DictionaryCasts.testSync("Preconcurrency") {

  typealias X = [String: () -> String]
  typealias Y = [AnyHashable: @MainActor @Sendable () -> String]

  let a: X = [
    "a": { "abc" },
    "b": { "xyz" },
    "c": { "ijk" }
  ]

  // expected-swift6-warning@+1 {{converting non-Sendable function value to '@MainActor @Sendable () -> String' may introduce data races}}
  let b: Y = takeDictionary(a)
  expectEqual(b.count, 3)
  expectEqual(b["a"]!(), "abc")
  expectEqual(b["b"]!(), "xyz")
  expectEqual(b["c"]!(), "ijk")

  // Despite the warning saying that this cast always succeeds, it actually fails
  // expected-warning@+1 {{conditional cast from 'X' (aka 'Dictionary<String, () -> String>') to 'Y' (aka 'Dictionary<AnyHashable, @MainActor @Sendable () -> String>') always succeeds}}
  let c = a as? Y
  expectNil(c)

  let d = (a as Any) as? Y
  expectNil(d)
}

await runAllTestsAsync()

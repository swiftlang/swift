// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -wmo -Onone -sil-verify-all %t/Lib.swift -module-name Lib -emit-module -emit-module-path %t/onone/Lib.swiftmodule -c -o %t/onone/Lib.o
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -wmo -Onone -sil-verify-all -I %t/onone %t/main.swift -module-name main -c -o %t/onone/main.o
// RUN: %target-embedded-link %t/onone/Lib.o %t/onone/main.o -o %t/onone/a.out %target-clang-resource-dir-opt -dead_strip
// RUN: %target-run %t/onone/a.out | %FileCheck %s

// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -wmo -O -sil-verify-all %t/Lib.swift -module-name Lib -emit-module -emit-module-path %t/o/Lib.swiftmodule -c -o %t/o/Lib.o
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -wmo -O -sil-verify-all -I %t/o %t/main.swift -module-name main -c -o %t/o/main.o
// RUN: %target-embedded-link %t/o/Lib.o %t/o/main.o -o %t/o/a.out %target-clang-resource-dir-opt -dead_strip
// RUN: %target-run %t/o/a.out | %FileCheck %s

// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -wmo -Osize -sil-verify-all %t/Lib.swift -module-name Lib -emit-module -emit-module-path %t/osize/Lib.swiftmodule -c -o %t/osize/Lib.o
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -wmo -Osize -sil-verify-all -I %t/osize %t/main.swift -module-name main -c -o %t/osize/main.o
// RUN: %target-embedded-link %t/osize/Lib.o %t/osize/main.o -o %t/osize/a.out %target-clang-resource-dir-opt -dead_strip
// RUN: %target-run %t/osize/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded

// In Embedded Swift, de-virtualizing deinits is mandatory, because there is no type metadata.
// Check that destroying public non-copyable types whose internal fields (or nested fields or
// enum payloads) have a deinit calls all deinits, also from a client module and from inlined
// @inlinable functions.
// https://github.com/swiftlang/swift/issues/94452

//--- Lib.swift

nonisolated(unsafe) public var deinitCount = 0

struct Inner: ~Copyable {
  var x: Int
  init() { x = 1 }
  deinit { deinitCount += 1 }
}

// Internal without deinit, but containing a field with a deinit.
struct Wrapper: ~Copyable {
  var inner = Inner()
  var y = 0
}

// The payload of `pair` is a tuple.
enum PairE: ~Copyable {
  case empty
  case pair(Inner, Int)
}

public struct Manager: ~Copyable {
  var inner: Inner
  public init() { inner = Inner() }
}

public struct Manager2: ~Copyable {
  var w = Wrapper()
  var z = 0
  public init() {}
}

public struct PairManager: ~Copyable {
  var e: PairE = .pair(Inner(), 0)
  public init() {}
}

public struct ArrayManager: ~Copyable {
  var arr: InlineArray<2, Inner> = InlineArray { _ in Inner() }
  public init() {}
}

@inlinable public func consumeInLib(_ m: consuming Manager) {}
@inlinable public func consumePairInLib(_ m: consuming PairManager) {}
@inlinable public func consumeArrayInLib(_ m: consuming ArrayManager) {}

//--- main.swift

import Lib

@inline(never) func c1(_ m: consuming Manager) {}
@inline(never) func c2(_ m: consuming Manager2) {}
@inline(never) func c3(_ m: consuming PairManager) {}
@inline(never) func c4(_ m: consuming ArrayManager) {}

final class Holder {
  var m = Manager()
  var m2 = Manager2()
}

c1(Manager())
c2(Manager2())
c3(PairManager())
c4(ArrayManager())
// CHECK: 5
print(deinitCount)

consumeInLib(Manager())
consumePairInLib(PairManager())
consumeArrayInLib(ArrayManager())
// CHECK-NEXT: 9
print(deinitCount)

do {
  let h = Holder()
  _ = h
}
// CHECK-NEXT: 11
print(deinitCount)

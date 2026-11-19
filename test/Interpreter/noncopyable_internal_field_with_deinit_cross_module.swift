// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-build-swift-dylib(%t/%target-library-name(Lib)) -Onone -module-name Lib -emit-module -emit-module-path %t/Lib.swiftmodule %t/Lib.swift
// RUN: %target-codesign %t/%target-library-name(Lib)
// RUN: %target-build-swift -Onone -I %t -L %t -lLib %t/Main.swift -o %t/main_onone %target-rpath(%t)
// RUN: %target-codesign %t/main_onone
// RUN: %target-run %t/main_onone %t/%target-library-name(Lib) | %FileCheck %s

// RUN: %target-build-swift-dylib(%t/%target-library-name(Lib)) -O -module-name Lib -emit-module -emit-module-path %t/Lib.swiftmodule %t/Lib.swift
// RUN: %target-codesign %t/%target-library-name(Lib)
// RUN: %target-build-swift -O -I %t -L %t -lLib %t/Main.swift -o %t/main_o %target-rpath(%t)
// RUN: %target-codesign %t/main_o
// RUN: %target-run %t/main_o %t/%target-library-name(Lib) | %FileCheck %s

// REQUIRES: executable_test

// A public non-copyable struct with internal non-copyable fields which have a deinit (directly or
// nested) must be destroyable from a client module. The internal types' metadata is not accessible
// from the client, so the destroy must not be decomposed into destroys of those fields.
// https://github.com/swiftlang/swift/issues/94452

//--- Lib.swift

nonisolated(unsafe) public var deinitCount = 0

struct Inner: ~Copyable {
  let p: UnsafeMutablePointer<Int>
  init() { p = .allocate(capacity: 1) }
  deinit {
    p.deallocate()
    deinitCount += 1
  }
}

struct Wrapper: ~Copyable {
  var inner = Inner()
  var s = "x"
}

public struct PublicField: ~Copyable {
  var inner = Inner()
  public init() {}
}

enum E: ~Copyable {
  case a(Inner)
  case b
}

public struct Manager: ~Copyable {
  var inner: Inner
  public init() { inner = Inner() }
}

public struct Manager2: ~Copyable {
  var w = Wrapper()
  var o: AnyObject? = nil
  public init() {}
}

public struct Manager3: ~Copyable {
  var pf = PublicField()
  var w = Wrapper()
  public init() {}
}

public struct Manager4: ~Copyable {
  var e: E = .a(Inner())
  public init() {}
}

// The payload of `pair` is a tuple.
enum PairE: ~Copyable {
  case empty
  case pair(Inner, Int)
}

public struct PairManager: ~Copyable {
  var e: PairE = .pair(Inner(), 0)
  public init() {}
}

@inlinable public func consumeInLib(_ m: consuming Manager) {}
@inlinable public func consumeInLib2(_ m: consuming Manager2) {}
@inlinable public func consumeInLib3(_ m: consuming Manager3) {}
@inlinable public func consumeInLib4(_ m: consuming Manager4) {}
@inlinable public func consumeInLib5(_ m: consuming PairManager) {}

@inlinable public func localInLib() {
  var m = Manager2()
  m = Manager2()
  _ = consume m
}

//--- Main.swift

import Lib

@inline(never) func c1(_ m: consuming Manager) {}
@inline(never) func c2(_ m: consuming Manager2) {}
@inline(never) func c3(_ m: consuming Manager3) {}
@inline(never) func c4(_ m: consuming Manager4) {}
@inline(never) func c5(_ m: consuming PairManager) {}

final class Holder {
  var m = Manager()
  var m2 = Manager2()
  var m3 = Manager3()
  var m4 = Manager4()
}

c1(Manager())
c2(Manager2())
c3(Manager3())
c4(Manager4())
c5(PairManager())
// CHECK: 6
print(deinitCount)

consumeInLib(Manager())
consumeInLib2(Manager2())
consumeInLib3(Manager3())
consumeInLib4(Manager4())
consumeInLib5(PairManager())
// CHECK-NEXT: 12
print(deinitCount)

localInLib()
// CHECK-NEXT: 14
print(deinitCount)

do {
  let h = Holder()
  _ = h
}
// CHECK-NEXT: 19
print(deinitCount)

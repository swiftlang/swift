// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// This test builds a module consisting of two files. One declares the type, and another
// declares extensions of those types containing initializers.
// https://github.com/swiftlang/swift/issues/91700

// RUN: %target-build-swift %t/Types.swift %t/Inits.swift -module-name Lib \
// RUN:   -emit-module -emit-module-path %t/Lib.swiftmodule \
// RUN:   -emit-library -static -o %t/%target-static-library-name(Lib)
// RUN: %target-build-swift %t/main.swift -I %t -L %t -l Lib -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test

// Init accessors are not fully supported yet under SIL opaque values
// XFAIL: swift_test_mode_optimize_none_with_opaque_values

//--- Types.swift

// 'facade' has a synthesized 'nil' default (it is Optional); it subsumes
// 'storage', so a cross-file init must yield nil, not storage's own 7.
public struct ImplicitDefault {
  var storage: Int? = 7
  var facade: Int? {
    @storageRestrictions(initializes: storage)
    init(initialValue) { storage = initialValue }
    get { storage }
  }
  public var read: Int? { storage }
}

// Two accessors initialize the same storage, both with defaults: the last wins.
public struct TwoDefaults {
  var storage = 0
  var facade1: Int = 520 {
    @storageRestrictions(initializes: storage)
    init(initialValue) { storage = initialValue }
    get { storage }
    set { storage = newValue }
  }
  var facade2: Int = 42 {
    @storageRestrictions(initializes: storage)
    init(initialValue) { storage = initialValue }
    get { storage }
    set { storage = newValue }
  }
  public var read: Int { storage }
}

// A class extension can only add a delegating convenience init cross-file, so the
// member initializers are emitted in the designated 'init()'; the
// synthesized-'nil' default of 'facade' still subsumes the default of 'storage'.
public class ImplicitDefaultClass {
  var storage: Int? = 7
  var facade: Int? {
    @storageRestrictions(initializes: storage)
    init(initialValue) { storage = initialValue }
    get { storage }
  }
  public init() {}
  public var read: Int? { storage }
}

public struct GenericHolder<T> {
  var tag: T
  var storage: Int? = 7
  var facade: Int? {
    @storageRestrictions(initializes: storage)
    init(initialValue) { storage = initialValue }
    get { storage }
  }
  public var read: Int? { storage }
}

// Non-subsumed control: 'facade' has no default, so it does NOT subsume
// 'storage'. The storage's own default value 99 is expected.
public struct NoDefault {
  var storage = 99
  var facade: Int {
    @storageRestrictions(initializes: storage)
    init(initialValue) { storage = initialValue }
    get { storage }
    set { storage = newValue }
  }
  public var read: Int { storage }
}

//--- Inits.swift

extension ImplicitDefault { public init(other: Int) {} }
extension TwoDefaults { public init(other: Int) {} }
extension ImplicitDefaultClass {
  public convenience init(other: Int) { self.init() }
}
// A non-delegating cross-file initializer: 'tag' is set explicitly and the
// member initializers must set up 'storage' through the accessor.
extension GenericHolder { public init(tag: T) { self.tag = tag } }
extension NoDefault { public init(other: Int) {} }

//--- main.swift

import Lib

print("ImplicitDefault:", ImplicitDefault(other: 0).read as Any)
// CHECK: ImplicitDefault: nil

print("TwoDefaults:", TwoDefaults(other: 0).read)
// CHECK: TwoDefaults: 42

print("ImplicitDefaultClass:", ImplicitDefaultClass(other: 0).read as Any)
// CHECK: ImplicitDefaultClass: nil

print("GenericHolder:", GenericHolder(tag: "x").read as Any)
// CHECK: GenericHolder: nil

print("NoDefault:", NoDefault(other: 0).read)
// CHECK: NoDefault: 99

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// The type is built in a library from two files (its declaration in a secondary
// file relative to the cross-file initializer); the client then runs it. This
// guards the runtime values, not just the emitted SIL: an init accessor whose
// (possibly synthesized) default subsumes a stored property must initialize that
// storage through the accessor, even from a constructor in another file.
// https://github.com/swiftlang/swift/issues/91700

// RUN: %target-build-swift %t/Types.swift %t/Inits.swift -module-name Lib \
// RUN:   -emit-module -emit-module-path %t/Lib.swiftmodule \
// RUN:   -emit-library -static -o %t/%target-static-library-name(Lib)
// RUN: %target-build-swift %t/main.swift -I %t -L %t -l Lib -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test

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

// A class: member-initializer emission differs from a struct's
// (checkClassConstructorBody is class-only), so guard that path too. A class
// extension can only add a delegating convenience init cross-file, so the
// member initializers are emitted in the designated 'init()'; the residual
// synthesized-'nil' default must still subsume 'storage' there, so 'read'
// is nil rather than storage's own 7.
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

// A generic type: same residual subsumption, exercising the generic SILGen path.
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
// 'storage'. A cross-file init must keep storage's own value (99), guarding
// against a regression that over-eagerly subsumes.
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
// A class extension can only add a convenience initializer cross-file; it
// delegates to the designated 'init()', whose member initializers must still
// route 'storage' through the accessor.
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

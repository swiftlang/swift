// REQUIRES: OS=macosx
// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck %s -emit-tbd -emit-tbd-path %t/before_move.tbd -D BEFORE_MOVE -module-name Foo -enable-library-evolution -emit-sil -o %t/before_move.sil
// RUN: %FileCheck %s < %t/before_move.tbd
// RUN: %FileCheck %s --check-prefix=CHECK-SIL < %t/before_move.sil

// RUN: %target-swift-frontend %s -emit-module -emit-module-path %t/FooCore.swiftmodule -D AFTER_MOVE_FOO_CORE -module-name FooCore -enable-library-evolution
// RUN: %target-swift-frontend -typecheck %s -emit-tbd -emit-tbd-path %t/after_move.tbd -D AFTER_MOVE_FOO -module-name Foo -I %t -enable-library-evolution -emit-sil -o %t/after_move.sil
// RUN: %FileCheck %s < %t/after_move.tbd
// RUN: %FileCheck %s --check-prefix=CHECK-SIL < %t/after_move.sil

// CHECK: '_$s3Foo4DateC14getCurrentYearSiyFZ'
// CHECK: '_$s3Foo9DateValueV4yearACSi_tcfC'
// CHECK: '_$s3Foo9DateValueV4yearSivg'

// CHECK-SIL: sil [available 10.7] @$s3Foo4DateC14getCurrentYearSiyFZ : $@convention(method) (@thick Date.Type) -> Int
// CHECK-SIL: sil [available 10.7] @$s3Foo9DateValueV4yearACSi_tcfC : $@convention(method) (Int, @thin DateValue.Type) -> @out DateValue
// CHECK-SIL: sil [available 10.7] @$s3Foo9DateValueV4yearSivg : $@convention(method) (@in_guaranteed DateValue) -> Int

#if BEFORE_MOVE

@available(OSX 10.7, *)
public class Date {
  public static func getCurrentYear() -> Int { return 2020 }
}

@available(OSX 10.7, *)
public struct DateValue {
  public init(year: Int) {}
  public var year: Int { return 2020 }
}

#endif

#if AFTER_MOVE_FOO_CORE

@available(OSX 10.7, *)
@_originallyDefinedIn(module: "Foo", OSX 10.9)
public class Date {}

@available(OSX 10.7, *)
@_originallyDefinedIn(module: "Foo", OSX 10.9)
public struct DateValue {}

#endif

#if AFTER_MOVE_FOO

@_exported import FooCore

public extension Date {
  public static func getCurrentYear() -> Int { return 2020 }
}

public extension DateValue {
  public init(year: Int) { self.init(year: 2020) }
  public var year: Int { return 2020 }
}

#endif

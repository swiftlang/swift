// REQUIRES: OS=macosx
// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck %s -emit-tbd -emit-tbd-path %t/before_move.tbd -D BEFORE_MOVE -tbd-install_name FooCore -module-name Foo -enable-library-evolution -emit-sil -o %t/before_move.sil

// RUN: %llvm-nm %t/before_move.tbd | %FileCheck %s --check-prefix=CHECK-TBD 
// RUN: %FileCheck %s --check-prefix=CHECK-SIL < %t/before_move.sil

// RUN: %target-swift-frontend %s -emit-module -emit-module-path %t/FooCore.swiftmodule -D AFTER_MOVE_FOO_CORE -module-name FooCore -enable-library-evolution -tbd-install_name FooCore -emit-tbd -emit-tbd-path %t/after_move.tbd -emit-sil -o %t/after_move.sil

// RUN: %llvm-nm %t/after_move.tbd | %FileCheck %s --check-prefix=CHECK-TBD 
// RUN: %FileCheck %s --check-prefix=CHECK-SIL < %t/after_move.sil

// CHECK-TBD: _$s3Foo4DateC03SubB0V4yearAESi_tcfC
// CHECK-TBD: _$s3Foo4DateC03SubB0VMn

// CHECK-SIL: sil [available 10.7] @$s3Foo4DateC03SubB0V4yearAESi_tcfC : $@convention(method) (Int, @thin Date.SubDate.Type) -> @out Date.SubDate

#if BEFORE_MOVE

@available(OSX 10.7, *)
public class Date {
  public static func getCurrentYear() -> Int { return 2020 }
}

@available(OSX 10.7, *)
extension Date {
  public struct SubDate {
    public init(year: Int) {}
  }
}

#endif

#if AFTER_MOVE_FOO_CORE

@available(OSX 10.7, *)
@_originallyDefinedIn(module: "Foo", OSX 10.9)
public class Date {}

@available(OSX 10.7, *)
@_originallyDefinedIn(module: "Foo", OSX 10.9)
extension Date {
  public struct SubDate {
    public init(year: Int) {}
  }
}

#endif

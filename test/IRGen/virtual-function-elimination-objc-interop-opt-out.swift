// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -enable-llvm-vfe -internalize-at-link -emit-ir %s -o %t/ir.ll
// RUN: %FileCheck --input-file %t/ir.ll %s --check-prefix=OBJC-BASE
// RUN: %FileCheck --input-file %t/ir.ll %s --check-prefix=OBJC-DERIVED
// RUN: %FileCheck --input-file %t/ir.ll %s --check-prefix=BASE
// RUN: %FileCheck --input-file %t/ir.ll %s --check-prefix=DERIVED
// RUN: echo '' > %t/used-symbols
// RUN: %target-build-swift -emit-library -lto=llvm-full %lto_flags -Xfrontend -enable-llvm-vfe -Xfrontend -internalize-at-link -Xlinker -exported_symbols_list -Xlinker %t/used-symbols %s -o %t/main
// RUN: %llvm-nm --defined-only %t/main | %FileCheck %s --check-prefix=NM

// REQUIRES: objc_interop
// REQUIRES: no_asan
// UNSUPPORTED: remote_run

// Tests that under -enable-llvm-vfe @objc virtual functions are not eligible for elimination

import Foundation

// Objective-C exposed types should have public vcall_visibility even under
// -internalize-at-link

@objc public class ObjCBase : NSObject {
    @objc public func foo() { print("ObjCBase.foo") }
    @objc public func bar() { print("ObjCBase.bar") }
}

// OBJC-BASE: @"$s2ir8ObjCBaseCMn" = {{.*}}!vcall_visibility ![[VCALL_VISIBILITY_METADATA_NODE:[0-9]+]]
// OBJC-BASE: ![[VCALL_VISIBILITY_METADATA_NODE]] = !{i64 0

@objc public class ObjCDerived : ObjCBase {
    @objc override public func foo() { print("ObjCDerived.foo") }
    @objc override public func bar() { print("ObjCDerived.bar") }
}

// OBJC-DERIVED: @"$s2ir11ObjCDerivedCMn" = {{.*}}!vcall_visibility ![[VCALL_VISIBILITY_METADATA_NODE:[0-9]+]]
// OBJC-DERIVED: ![[VCALL_VISIBILITY_METADATA_NODE]] = !{i64 0

// Regular Swift types should have linkage unit vcall_visibility

public class Base {
    public func foo() { print("Base.foo") }
    public func bar() { print("Base.bar") }
}

// BASE: @"$s2ir4BaseCMn" = {{.*}}!vcall_visibility ![[VCALL_VISIBILITY_METADATA_NODE:[0-9]+]]
// BASE: ![[VCALL_VISIBILITY_METADATA_NODE]] = !{i64 1

public class Derived : Base {
    override public func foo() { print("Derived.foo") }
    override public func bar() { print("Derived.bar") }
}

// DERIVED: @"$s2ir7DerivedCMn" = {{.*}}!vcall_visibility ![[VCALL_VISIBILITY_METADATA_NODE:[0-9]+]]
// DERIVED: ![[VCALL_VISIBILITY_METADATA_NODE]] = !{i64 1

// NM:     $s4main11ObjCDerivedC3baryyF{{$}}
// NM:     $s4main11ObjCDerivedC3fooyyF{{$}}
// NM-NOT: $s4main4BaseC3baryyF{{$}}
// NM-NOT: $s4main4BaseC3fooyyF{{$}}
// NM-NOT: $s4main7DerivedC3baryyF{{$}}
// NM-NOT: $s4main7DerivedC3fooyyF{{$}}
// NM:     $s4main8ObjCBaseC3baryyF{{$}}
// NM:     $s4main8ObjCBaseC3fooyyF{{$}}

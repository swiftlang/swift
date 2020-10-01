// Please keep this file in alphabetical order!

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/resilient_struct.swiftmodule %S/../Inputs/resilient_struct.swift -enable-library-evolution
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module-path %t/resilient_objc_class.swiftmodule %S/../Inputs/resilient_objc_class.swift -I %t -enable-library-evolution -emit-objc-header-path %t/resilient_objc_class.h

// RUN: cp %S/Inputs/custom-modules/module.map %t/module.map

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %s -module-name resilient -emit-objc-header-path %t/resilient.h -I %t -enable-library-evolution
// RUN: %FileCheck %s --check-prefix=NO-STUBS < %t/resilient.h
// RUN: %check-in-clang %t/resilient.h -I %t

// REQUIRES: objc_interop
// UNSUPPORTED: OS=iosmac

// See also resilient-ancestry.swift, for the stable ABI deployment target test.

import Foundation
import resilient_objc_class

// Note: @_fixed_layout on a class only applies to the storage layout and
// not metadata, which remains resilient.

// NO-STUBS-NOT: FixedLayoutNSObjectSubclass

// CHECK-LABEL: SWIFT_RESILIENT_CLASS("_TtC9resilient27FixedLayoutNSObjectSubclass")
// CHECK-NEXT: @interface FixedLayoutNSObjectSubclass : FixedLayoutNSObjectOutsideParent
// CHECK-NEXT:   - (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
// CHECK-NEXT: @end

@_fixed_layout
public class FixedLayoutNSObjectSubclass : FixedLayoutNSObjectOutsideParent {}

// NO-STUBS-NOT: ResilientNSObjectSubclass

// CHECK-LABEL: SWIFT_RESILIENT_CLASS("_TtC9resilient25ResilientNSObjectSubclass")
// CHECK-NEXT: @interface ResilientNSObjectSubclass : ResilientNSObjectOutsideParent
// CHECK-NEXT:   - (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
// CHECK-NEXT: @end

public class ResilientNSObjectSubclass : ResilientNSObjectOutsideParent {}

// NO-STUBS-NOT: RenamedNSObjectSubclass

// CHECK-LABEL: SWIFT_RESILIENT_CLASS_NAMED("UnrenamedNSObjectSubclass")
// CHECK-NEXT: @interface RenamedNSObjectSubclass : ResilientNSObjectOutsideParent
// CHECK-NEXT:   - (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
// CHECK-NEXT: @end

@objc(RenamedNSObjectSubclass)
public class UnrenamedNSObjectSubclass : ResilientNSObjectOutsideParent {}

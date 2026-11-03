// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-build-swift-dylib(%t/%target-library-name(ConstrainedBase)) %t/base.swift -emit-module -emit-module-path %t/ConstrainedBase.swiftmodule -module-name ConstrainedBase
// RUN: %target-codesign %t/%target-library-name(ConstrainedBase)

// RUN: %target-build-swift -lswiftSwiftReflectionTest %t/main.swift -L %t -I %t -lConstrainedBase -o %t/reflect_constrained_extension_generic_parent %target-rpath(%t)
// RUN: %target-codesign %t/reflect_constrained_extension_generic_parent

// RUN: %target-run %target-swift-reflection-test %t/reflect_constrained_extension_generic_parent | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: reflection_test_support
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan
// XFAIL: *

// REMOTE-MIRRORS-GAP-BEGIN
// Title: cross-module constrained extension on a generic parent — descriptor lookup bails before DemanglingForTypeRef
// Status: XFAIL
// RemoteInspection-Reference: stdlib/public/RemoteInspection/TypeRef.cpp Extension handling in DemanglingForTypeRef::substituteParentIntoContext (around line 645) was extended on this branch (commit 0d82d03e) to handle the 3-children Extension shape (Module, ExtendedType, DependentGenericSignature), so the visitor can rebuild the mangling once it is reached. The remaining gap is upstream of the visitor: TypeRefBuilder::ReflectionTypeDescriptorFinder cannot navigate the constrained-extension context to locate the class descriptor for `Outer<T>.Inner where T: Equatable` declared in a different module from `Outer`. Both swift_reflection_projectExistential and swift_reflection_typeRefForInstance bail before DemanglingForTypeRef ever runs.
// Runtime-Reference: stdlib/public/runtime/MetadataLookup.cpp — the in-process runtime walks the generic context and resolves nested extension contexts using TargetExtensionContextDescriptor and signature substitution, so `Outer<Int>.Inner` resolves normally for the in-process Mirror.
// Description:
//   Two modules: ConstrainedBase exports `public struct Outer<T>`; the test
//   module declares `extension Outer where T: Equatable { public class Inner }`.
//   reflect(any: Outer<Int>.Inner()) prints
//   `swift_reflection_projectExistential failed.` (existential projection
//   path), and reflect(object: Outer<Int>.Inner()) reads the object header
//   but reports `<null type reference>` / `<null type info>` (object path).
//   In both cases, descriptor lookup gives up before DemanglingForTypeRef
//   runs. Closing the gap means teaching the descriptor finder to walk
//   constrained extension contexts on a generic parent across modules.
// REMOTE-MIRRORS-GAP-END

//--- base.swift

public struct Outer<T> {
  public init() {}
}

//--- main.swift

import ConstrainedBase
import SwiftReflectionTest

extension Outer where T: Equatable {
  public class Inner {
    public var x: Int = 42
    public init() {}
  }
}

reflect(object: Outer<Int>.Inner())

// CHECK: Reflecting an object.
// CHECK-NOT: <null type reference>
// CHECK-NOT: <null type info>
// CHECK: Type reference:
// CHECK: (bound_generic_class {{.*}}Outer{{.*}}Inner
// CHECK: Type info:
// CHECK: (class_instance
// CHECK: (field name=x

reflect(any: Outer<Int>.Inner())

// CHECK: Reflecting an existential.
// CHECK-NOT: swift_reflection_projectExistential failed.
// CHECK: Type reference:
// CHECK: (bound_generic_class {{.*}}Outer{{.*}}Inner
// CHECK: Type info:
// CHECK: (reference

doneReflecting()

// CHECK: Done.

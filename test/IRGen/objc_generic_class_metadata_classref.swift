// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -emit-ir | %FileCheck %s

// REQUIRES: objc_interop

// Verify that ObjC classes referenced only through mangled type metadata
// strings (e.g., in a generic function instantiation) get a linker-visible
// classref entry emitted. This prevents a runtime crash when linking
// static archives without -ObjC, where the linker may not pull in the
// archive member defining the class since only the Swift runtime (via
// objc_getClass) actually references it.
//
// https://github.com/swiftlang/swift/issues/85441

import Foundation
import objc_generics

// CHECK-DAG: @"OBJC_CLASS_REF_$_GenericClass" = {{.*}}section "__DATA,__objc_classrefs

func useMetadata<T>(_ t: T.Type) {}

public func testObjCClassInMetadataRef() {
  // This triggers emitMetadataAccessByMangledName for Optional<GenericClass>
  // which should cause an __objc_classrefs entry for GenericClass
  useMetadata(Optional<GenericClass<NSString>>.self)
}

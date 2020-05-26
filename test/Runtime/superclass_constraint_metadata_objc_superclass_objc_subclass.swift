// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-library -enable-library-evolution -module-name Framework -module-link-name Framework %S/Inputs/public_struct_with_generic_arg_nsobject_constrained.swift -o %t/%target-library-name(Framework) -emit-module-path %t/Framework.swiftmodule
// RUN: %target-codesign %t/libFramework.dylib

// RUN: %target-clang -fobjc-arc %S/Inputs/SubclassOfNSObject.m -c -o %t/Superclass.o
// RUN: %target-build-swift %s %t/Superclass.o %S/Inputs/print_subclass/main.swift -import-objc-header %S/Inputs/SubclassOfNSObject.h -module-name main -o %t/main -I %t -L %t
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %S/Inputs/print_subclass/main.swift

// REQUIRES: objc_interop
// REQUIRES: executable_test
// REQUIRES: OS=macosx
// UNSUPPORTED: use_os_stdlib

import Swift
import Foundation
import Framework

// ObjC subclass of an ObjC subclass

// isSubclass is never called: no reference to subclass metadata from subclass
// metadata; i.e. Subclass has no member of type Gen<Subclass>?

@inline(never)
public func consume<T>(_ t: T) {
  withExtendedLifetime(t) { t in
  }
}

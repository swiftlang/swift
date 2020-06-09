// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-library -enable-library-evolution -module-name Framework -module-link-name Framework %S/Inputs/public_struct_with_generic_arg_nsobject_constrained.swift -o %t/%target-library-name(Framework) -emit-module-path %t/Framework.swiftmodule
// RUN: %target-codesign %t/libFramework.dylib

// RUN: %target-clang -fobjc-arc %S/Inputs/Superclass.m -c -o %t/Superclass.o
// RUN: %target-build-swift %s %t/Superclass.o %S/Inputs/print_subclass/main.swift -import-objc-header %S/Inputs/Superclass.h -module-name main -o %t/main -I %t -L %t
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %S/Inputs/print_subclass/main.swift

// REQUIRES: objc_interop
// REQUIRES: executable_test
// REQUIRES: OS=macosx
// UNSUPPORTED: use_os_stdlib

import Swift
import Foundation
import Framework

// Swift subclass of an ObjC subclass of an ObjC class

// subclass isSwiftClassMetadataSubclass metadata completeness : Complete
// superclass metadata path: loop
// iteration 1: subclass->Superclass == NSObject
//              subclass <= NSObject
//              superclass == NSObject; done

typealias Gen = Framework.Gen<Subclass>

public class Subclass : Superclass {
  override init() {
    self.gen = Gen()
    super.init()
  }

  var gen: Gen?
}

@inline(never)
public func consume<T>(_ t: T) {
  withExtendedLifetime(t) { t in
  }
}

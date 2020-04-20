// REQUIRES: rdar61345988
// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-library -enable-library-evolution -module-name Framework -module-link-name Framework %S/Inputs/public_struct_with_generic_arg_swift_class_constrained.swift -o %t/%target-library-name(Framework) -emit-module-path %t/Framework.swiftmodule -target %module-target-future
// RUN: %target-codesign %t/libFramework.dylib

// RUN: %target-build-swift %s %S/Inputs/print_subclass/main.swift -module-name main -o %t/main -I %t -L %t -target %module-target-future
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %S/Inputs/print_subclass/main.swift

// REQUIRES: executable_test
// REQUIRES: rdar61345988
// REQUIRES: OS=macosx
// UNSUPPORTED: use_os_stdlib

import Swift
import Framework

// Swift subclass of a Swift class

// subclass isSwiftClassMetadataSubclass metadata completeness : LayoutComplete
// superclass metadata path: getSuperclassMetadata
// getSuperclassMetadata result 1: (Complete, Superclass)
// subclass == superclass; done

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

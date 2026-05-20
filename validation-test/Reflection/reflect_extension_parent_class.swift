// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-build-swift-dylib(%t/%target-library-name(Base)) %t/base.swift -emit-module -emit-module-path %t/Base.swiftmodule -module-name Base
// RUN: %target-codesign %t/%target-library-name(Base)

// RUN: %target-build-swift -lswiftSwiftReflectionTest %t/main.swift -L %t -I %t -lBase -o %t/reflect_extension_parent_class %target-rpath(%t)
// RUN: %target-codesign %t/reflect_extension_parent_class

// RUN: %target-run %target-swift-reflection-test %t/reflect_extension_parent_class | tee /dev/stderr | %FileCheck %s --dump-input=fail

// REQUIRES: executable_test
// REQUIRES: reflection_test_support
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

// Verifies that reflection can resolve types declared inside a cross-module
// extension — i.e., types whose parent (the extended type) lives in a
// different module than the extension itself.

//--- base.swift

public struct Outer {
  public init() {}
}

//--- main.swift

import Base
import SwiftReflectionTest

extension Outer {
  public class Inner<Content> {
    public let field: Content
    public init(_ v: Content) { self.field = v }
  }

  // Non-generic struct in an unconstrained cross-module extension.
  public struct Plain {
    public let flag: Int = 99
  }
}

reflect(any: Outer.Inner<Int>(42))

// CHECK: Reflecting an existential.
// CHECK: Type reference:
// CHECK: (bound_generic_class (extension in reflect_extension_parent_class):Base.Outer.Inner
// CHECK: Mangled name: $s4Base5OuterV{{.*}}extension_parent_classE5InnerC{{.*}}
// CHECK: Demangled name: (extension in reflect_extension_parent_class):Base.Outer.Inner<Swift.Int>

reflect(any: Outer.Plain())

// CHECK: Reflecting an existential.
// CHECK: Type reference:
// CHECK: (struct (extension in reflect_extension_parent_class):Base.Outer.Plain
// CHECK: Mangled name: $s4Base5OuterV{{.*}}extension_parent_classE5PlainV{{.*}}
// CHECK: Demangled name: (extension in reflect_extension_parent_class):Base.Outer.Plain

doneReflecting()

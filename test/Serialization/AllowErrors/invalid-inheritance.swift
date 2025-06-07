// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/mods)

// RUN: touch %t/empty.swift
// RUN: %{python} %utils/split_file.py -o %t %s

// Errors often only occur during merging, hence creating an empty module here
// RUN: %target-swift-frontend -verify -module-name errors -emit-module -o %t/mods/errorsmain.partial.swiftmodule -experimental-allow-module-with-compiler-errors %t/errors.swift
// RUN: %target-swift-frontend -module-name errors -emit-module -o %t/mods/errorsempty.partial.swiftmodule %t/empty.swift
// RUN: %target-swift-frontend -module-name errors -emit-module -o %t/mods/errors.swiftmodule -experimental-allow-module-with-compiler-errors %t/mods/errorsmain.partial.swiftmodule %t/mods/errorsempty.partial.swiftmodule

// RUN: %target-swift-frontend -emit-module -o %t/mods/uses.swiftmodule -experimental-allow-module-with-compiler-errors -I %t/mods %t/uses.swift 2>&1 | %FileCheck -check-prefix=CHECK-USES %s

// BEGIN errors.swift
public protocol SomeProto: undefined {} // expected-error {{cannot find type 'undefined'}}
public class SomeClass: undefined {} // expected-error {{cannot find type 'undefined'}}
public struct SomeStruct: undefined {} // expected-error {{cannot find type 'undefined'}}
public enum SomeEnum: undefined { // expected-error {{cannot find type 'undefined'}}
  case a
}

public class GenericClass<T> {}
public class InvalidGenericSuperclass: GenericClass<undefined> {} // expected-error {{cannot find type 'undefined'}}

extension SomeClass: undefined {} // expected-error {{cannot find type 'undefined'}}
extension SomeStruct: undefined {} // expected-error {{cannot find type 'undefined'}}
extension SomeEnum: undefined {} // expected-error {{cannot find type 'undefined'}}

extension undefined {} // expected-error {{cannot find type 'undefined'}}
extension undefined: undefined {} // expected-error 2 {{cannot find type 'undefined'}}
extension undefined: SomeProto {} // expected-error {{cannot find type 'undefined'}}

public extension undefined { // expected-error {{cannot find type 'undefined' in scope}}
  protocol SomeProtoInner: undefined {} // expected-error {{cannot find type 'undefined' in scope}}
  class SomeClassInner: undefined {} // expected-error {{cannot find type 'undefined' in scope}}
  struct SomeStructInner: undefined {} // expected-error {{cannot find type 'undefined' in scope}}
  enum SomeEnumInner: undefined { // expected-error {{cannot find type 'undefined' in scope}}
    case a
  }
  class InvalidGenericSuperclassInner: GenericClass<undefined> {} // expected-error {{cannot find type 'undefined' in scope}}
}


// BEGIN uses.swift
import errors
func test(p: SomeProto, c: SomeClass, s: SomeStruct, e: SomeEnum, g: InvalidGenericSuperclass) {}
// CHECK-USES-NOT: cannot find type 'SomeProto' in scope
// CHECK-USES-NOT: cannot find type 'SomeClass' in scope
// CHECK-USES-NOT: cannot find type 'SomeStruct' in scope
// CHECK-USES-NOT: cannot find type 'SomeEnum' in scope
// CHECK-USES-NOT: cannot find type 'InvalidGenericSuperclass' in scope

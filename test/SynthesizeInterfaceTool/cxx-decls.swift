// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-synthesize-interface -module-name CxxDecls -I %t/Inputs -cxx-interoperability-mode=default | %FileCheck %s
// RUN: %target-swift-synthesize-interface -module-name CxxDeclsExtra -I %t/Inputs -cxx-interoperability-mode=default | %FileCheck %s --check-prefix=EXTRA

//--- Inputs/module.modulemap
module CxxDecls {
  requires cplusplus
  header "cxx-decls.h"
}

module CxxDeclsExtra {
  requires cplusplus
  header "cxx-decls-extra.h"
}

//--- Inputs/cxx-decls.h
#pragma once

struct Counter {
  int value;
  Counter operator+(Counter rhs) const { return Counter{value + rhs.value}; }
  Counter &operator++() {
    ++value;
    return *this;
  }
};
// `operator+` on a C++ struct is imported as a synthesized `static func +`
// CHECK: public struct Counter {
// CHECK:     public static func + (lhs: Counter, rhs: Counter) -> Counter
// CHECK:     public func successor() -> Counter
// CHECK: }

struct Iter {
  int *ptr;
  int &operator*() const { return *ptr; }
};

// `operator*()` is imported as a synthesized `pointee` property
// CHECK: public struct Iter {
// CHECK:     public var pointee: CInt
// CHECK: }

struct Base {
  int baseField;
  void baseMethod() const {}
};

struct Derived : Base {
  int derivedField;
};

// C++ public inheritance: `Derived` exposes `Base`'s field and method
// alongside its own.
// CHECK: public struct Derived {
// CHECK:     public var baseField: CInt
// CHECK:     public func baseMethod()
// CHECK:     public var derivedField: CInt
// CHECK: }

enum class Status : int {
  Active,
  Inactive,
};

// A C++ scoped enum imports as a Swift enum with a synthesized
// `init?(rawValue:)`, `rawValue` property, and `RawValue` typealias.
// CHECK: public enum Status : CInt {
// CHECK:     public init?(rawValue: CInt)
// CHECK:     public var rawValue: CInt { get }
// CHECK:     public typealias RawValue = CInt
// CHECK:     case Active
// CHECK:     case Inactive
// CHECK: }

namespace OuterNS {
// A C++ namespace imports as a Swift enum at the module top level.
// CHECK: public enum OuterNS {

int globalFn(int x);
// CHECK: public static func globalFn(_ x: CInt) -> CInt

struct Nested {
  int method() const { return field; }
  int field;
};
// CHECK: public struct Nested {
// CHECK:     public func method() -> CInt
// CHECK:     public var field: CInt
// CHECK: }

namespace InnerNS {
  int innerFn();
}
// CHECK: public enum InnerNS {
// CHECK:     public static func innerFn() -> CInt
// CHECK: }

} // namespace OuterNS
// CHECK: }

namespace EmptyNS {}
// Empty namespaces do not appear in output
// CHECK-NOT: EmptyNS

namespace SharedNS {
  void fn1();
  void fnShared();
}

// CxxDecls should only see its own contributions to `SharedNS`, not `fn2`
// which is defined in another module
// CHECK: enum SharedNS {
// CHECK-NOT: fn2
// CHECK:     func fn1()
// CHECK:     func fnShared()
// CHECK: }

//--- Inputs/cxx-decls-extra.h
#pragma once

namespace SharedNS {
  void fn2();
  void fnShared();
}

#include <cxx-decls.h>

// CxxDeclsExtra should only see its own contributions to `SharedNS`,
// even if CxxDeclsExtra imports CxxDecls (which declares `SharedNS::fn1`)
// EXTRA: enum SharedNS {
// EXTRA-NOT: fn1
// EXTRA:     func fn2()
// EXTRA:     func fnShared()
// EXTRA: }

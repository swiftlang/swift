// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(ReflectDependentModule)) -enable-library-evolution -module-name ReflectDependentModule -emit-module -emit-module-path %t/ReflectDependentModule.swiftmodule %S/Inputs/reflect_dependent_module.swift
// RUN: %target-build-swift -I%t -L%t -lReflectDependentModule -lswiftSwiftReflectionTest -o %t/reflect_dependent %target-rpath(%t) %s
// RUN: %target-codesign %t/reflect_dependent %t/%target-library-name(ReflectDependentModule)
// RUN: %target-run %target-swift-reflection-test %t/reflect_dependent | tee /dev/stderr | %FileCheck %s --check-prefix=CHECK-%target-ptrsize

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib


// REQUIRES: executable_test

import SwiftReflectionTest

import ReflectDependentModule

/*
protocol P {
  associatedtype D
}

class B<T>: P {
  typealias D = T
}

class C<T: P>: P {
  typealias D = T
}

class A<T: P> where T.D: P {
  enum E {
  case t(T)
  case td(T.D)
  case tdd(T.D.D)
  case none
  }
  var b : E
  init(b: T.D.D) { self.b = .tdd(b) }
}

var obj = A<C<B<Int>>>(b: 17)
reflect(object: obj)
*/


protocol P {
  associatedtype A
}
protocol Q {}

enum C<T: P & Q> {
case a(T.A)
case b(Int)
case c(String)
case d
case t(T)
}

struct S: P & Q {
  typealias A = ()
}


reflect(enum: C<S>.b(7))


doneReflecting()

// CHECK-64: Done.

// CHECK-32: Done.

// RUN: %target-swift-frontend -emit-silgen -verify -primary-file %s %S/Inputs/circularity_multifile_error_helper.swift

// SR-4594

struct A {
  var b: AnUndefinedType // expected-error {{use of undeclared type 'AnUndefinedType'}}
}

struct B {
  var a : External
}

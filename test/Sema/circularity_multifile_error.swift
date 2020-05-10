// RUN: %target-swift-frontend -emit-silgen -verify -primary-file %s %S/Inputs/circularity_multifile_error_helper.swift

// SR-4594

struct A {
  var b: AnUndefinedType // expected-error {{cannot find type 'AnUndefinedType' in scope}}
}

struct B {
  var a : External
}

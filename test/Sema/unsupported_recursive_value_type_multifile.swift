// RUN: %target-swift-frontend -emit-silgen -verify -primary-file %s %S/Inputs/unsupported_recursive_value_type_multifile_helper.swift

struct A {
  var b: B?
  // expected-error@-1 {{value type 'A' cannot have a stored property that recursively contains it}}
  // expected-note@-2 {{cycle beginning here: B? -> (some(_:): B) -> (a: A)}}
}

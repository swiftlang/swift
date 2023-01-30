// RUN: %target-swift-frontend -emit-silgen -verify -primary-file %s %S/Inputs/circularity_multifile_error_helper.swift

// https://github.com/apple/swift/issues/47171

struct A {
  var b: AnUndefinedType // expected-error {{cannot find type 'AnUndefinedType' in scope}}
}

struct B {
  var a : External
}

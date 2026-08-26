// RUN: %target-typecheck-verify-swift -verify-additional-file %S/Inputs%{fs-sep}function-templates.h -verify-ignore-unrelated -I %S/Inputs -enable-experimental-cxx-interop

// README: If you just added support for protocol composition to the
// ClangTypeConverter, please update this test to use a different type that we
// don't support so the error messages here are still tested.

import FunctionTemplates

// Make sure we don't import non-type template parameters.
public func callIntegerTemplates() {
  hasNonTypeTemplateParameter()               // expected-error {{cannot find 'hasNonTypeTemplateParameter' in scope}}
  hasDefaultedNonTypeTemplateParameter()      // expected-error {{cannot find 'hasDefaultedNonTypeTemplateParameter' in scope}}
}

public func callLvalueRef() {
  lvalueReference(true)                       // expected-error {{cannot pass immutable value as inout argument: literals are not mutable}}
}

// Use protocol composition to create a type that we cannot (yet) turn into a clang::QualType.
public protocol A { }
public protocol B { }
public protocol C { }

public func caller1(x: A & B) -> A & B {
  return passThrough(x)
}

public func caller2(x: A & B, y: A & C) -> A & B {
  return addMixedTypeParams(x, y)
}

// Make sure we emit an error and don't crash when failing to instantiate a function.
public func callexpectsConstCharPtr() {
  // expected-note@<unknown> {{in instantiation of function template specialization 'expectsConstCharPtr<int>' requested here}}
  expectsConstCharPtr(0 as Int32)
}

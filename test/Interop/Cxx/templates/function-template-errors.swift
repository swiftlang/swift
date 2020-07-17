// RUN: not %target-swift-emit-sil %s -I %S/Inputs -enable-cxx-interop 2>&1 | %FileCheck %s

// README: If you just added support for protocol composition to the
// ClangTypeConverter, please update this test to use a different type that we
// don't support so the error messages here are still tested.


import FunctionTemplates

// Use protocol composition to create a type that we cannot (yet) turn into a clang::QualType.
protocol A { }
protocol B { }
protocol C { }

// CHECK: error: could not generate C++ types from the generic Swift types provided. The following Swift type(s) provided to 'passThrough' were unable to be converted: A & B.
public func caller1(x: A & B) -> A & B {
  return passThrough(x)
}

// CHECK: error: could not generate C++ types from the generic Swift types provided. The following Swift type(s) provided to 'addTwoTemplates' were unable to be converted: A & B, A & C.
public func caller2(x: A & B, y: A & C) -> A & B {
  return addTwoTemplates(x, y)
}

// Make sure we emit an error and don't crash when failing to instantiate a function.
// CHECK: error: no matching function for call to 'takesString'
// CHECK: note: in instantiation of function template specialization 'expectsString<int>' requested here
// CHECK: note: candidate function not viable: no known conversion from 'int' to 'const char *' for 1st argument
public func callExpectsString() {
  expectsString(0 as Int32)
}

// Make sure we don't import non-type template parameters.
// CHECK: error: cannot find 'integerTemplate' in scope
// CHECK: error: cannot find 'defaultIntegerTemplate' in scope
public func callIntegerTemplates() {
  integerTemplate()
  defaultIntegerTemplate()
}

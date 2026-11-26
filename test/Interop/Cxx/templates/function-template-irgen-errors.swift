// RUN: not %target-swift-emit-ir %s -I %S/Inputs -cxx-interoperability-mode=default 2>&1 | %FileCheck %s

// Function template specializations are instantiated when IRGen emits them, so
// errors in their bodies are reported then.

import FunctionTemplates

// Make sure we emit an error and don't crash when failing to instantiate a function.
// CHECK: error: no matching function for call to 'takesString'
// CHECK: note: candidate function not viable: no known conversion from 'int' to 'const char * _Null_unspecified' for 1st argument
// CHECK: note: in instantiation of function template specialization 'expectsConstCharPtr<int>' requested here
public func callexpectsConstCharPtr() {
  expectsConstCharPtr(0 as Int32)
}

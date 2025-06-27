// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=upcoming-swift

import ImportAsMember

func takesNestedStruct(_ s: MyNS.NestedStruct) {
  _ = s.method()
  _ = s.methodConstRef()

  _ = nestedStruct_method(s) // expected-error {{'nestedStruct_method' has been replaced by instance method 'MyNS.NestedStruct.method()'}}
}

func takesDeepNestedStruct(_ s: MyNS.MyDeepNS.DeepNestedStruct) {
  _ = s.method()
  _ = s.methodConstRef()
  _ = s.methodTypedef()
  _ = s.methodTypedefQualName()

  _ = deepNestedStruct_method(s) // expected-error {{'deepNestedStruct_method' has been replaced by instance method 'MyNS.MyDeepNS.DeepNestedStruct.method()'}}
}

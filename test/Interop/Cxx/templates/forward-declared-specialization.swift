// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default

import ForwardDeclaredSpecialization

func testForwardDeclaredSpecialization(_ param: MyIntTemplate) { // expected-error {{cannot find type 'MyIntTemplate' in scope}}
}

func testCompleteSpecialization(_ param: MyCompleteIntTemplate) {
}

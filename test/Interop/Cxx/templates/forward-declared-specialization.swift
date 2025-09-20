// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default

import ForwardDeclaredSpecialization

func testForwardDeclaredSpecialization(_ param: MyIntTemplate) { 
  // expected-error@-1 {{cannot find type 'MyIntTemplate' in scope}}
}

func testCompleteSpecialization(_ param: MyCompleteDoubleTemplate) {
}

func testTypedefWithoutSpecialization(_ param: MyFloatTemplate) {
}

func testTypedefWithoutSpecialization2(_ param: MyCompleteBoolTemplate) {
}

func testLaterDefinedSpecialization(_ param: MyCharTemplate) {
}

func testLaterDefinedSpecialization2(_ param: MyCharTemplate2) {
}

// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default -suppress-remarks -suppress-notes

import ForwardDeclaredSpecialization

func testForwardDeclaredSpecialization(_ param: ForwardDeclaredInt) {
  // expected-error@-1 {{cannot find type 'ForwardDeclaredInt' in scope}}
}

func testCompleteSpecialization(_ param: CompleteDouble) {
  let _ = param.getValue()
}

func testSpecializationDefinedAfter(_ param: FloatTypedef) {
  let _ = param.value
}

func testForwardDeclaredStruct(_ param: ForwardDeclaredStructType) {
  // expected-error@-1 {{cannot find type 'ForwardDeclaredStructType' in scope}}
}

func testCompleteStruct(_ param: CompleteStructType) {
  let _ = param.value
}

func testForwardDeclaredPartial(_ param: ForwardDeclaredPartial) {
  // expected-error@-1 {{cannot find type 'ForwardDeclaredPartial' in scope}}
}

func testCompletePartial(_ param: CompletePartial) {
  let _ = param.ptr
  let _ = param.value
}

func testFunctionsUsingIncompleteSpec() {
  let inc = ReturnsIncompleteSpecialization()
  // expected-error@-1 {{return type is unavailable in Swift}}
  // expected-warning@-2 {{constant 'inc' inferred to have type 'Never', which is an enum with no cases}}
  TakesIncompleteSpecialization(inc)
  // expected-error@-1 {{cannot find 'TakesIncompleteSpecialization' in scope}}
}

func testFunctionsUsingPtrToIncompleteSpec(_ ptr: OpaquePointer) {
  let incPtr = ReturnsPtrToIncompleteSpecialization()
  TakesPtrToIncompleteSpecialization(incPtr)
  TakesPtrToIncompleteSpecialization(ptr)
}

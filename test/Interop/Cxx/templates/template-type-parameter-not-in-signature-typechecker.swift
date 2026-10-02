// RUN: %target-typecheck-verify-swift -I %S%{fs-sep}Inputs -cxx-interoperability-mode=default -strict-memory-safety -verify-additional-file %S%{fs-sep}Inputs%{fs-sep}template-type-parameter-not-in-signature.h

import TemplateTypeParameterNotInSignature

public func callMemberFunctionTemplates(_ s: Struct) {
  s.templateTypeParamNotUsedInSignature(T: Int.self)
  let _: Int = s.templateTypeParamUsedInReturnType(0)
}

// The thunk that drops the metatype arguments has the attributes of the
// specialization it calls.
public nonisolated func callMemberFunctionTemplatesWithAttributes(
    _ s: StructWithAttributes
) {
  s.templateTypeParamNotUsedInSignatureResult(T: Int.self)
  s.templateTypeParamNotUsedInSignatureDeprecated(T: Int.self)
  // expected-warning@-1 {{'templateTypeParamNotUsedInSignatureDeprecated(T:)' is deprecated: use something else}}
  s.templateTypeParamNotUsedInSignatureUnavailable(T: Int.self)
  // expected-error@-1 {{'templateTypeParamNotUsedInSignatureUnavailable(T:)' is unavailable: not here}}
  s.templateTypeParamNotUsedInSignatureUnsafe(T: Int.self)
  // expected-warning@-1 {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe instance method 'templateTypeParamNotUsedInSignatureUnsafe(T:)'}}
  s.templateTypeParamNotUsedInSignatureMainActor(T: Int.self)
  // expected-warning@-1 {{call to main actor-isolated instance method 'templateTypeParamNotUsedInSignatureMainActor(T:)' in a synchronous nonisolated context}}
  // So does the declaration rebuilt for an 'Int' result.
  let _: Int = s.templateTypeParamUsedInReturnTypeUnsafe(0)
  // expected-warning@-1 {{expression uses unsafe constructs but is not marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe instance method 'templateTypeParamUsedInReturnTypeUnsafe'}}
}

public func callMutableMemberFunctionTemplate(_ s: inout Struct) {
  s.templateTypeParamNotUsedInSignatureMutable(T: Int.self)
}

public func callStaticMemberFunctionTemplate() {
  Struct.templateTypeParamNotUsedInSignatureStatic(T: Int.self)
}

public func callFreeFunctionTemplates() {
  let _: Bool = templateTypeParamNotUsedInSignature(T: Int.self)
  multiTemplateTypeParamNotUsedInSignature(T: Float.self, U: Int.self)
  let _: Int = multiTemplateTypeParamOneUsedInSignature(1, T: Int.self)
  multiTemplateTypeParamNotUsedInSignatureWithUnrelatedParams(
      1, 1, T: Int32.self, U: Int.self)
  let _: Int = templateTypeParamUsedInReturnType(10)
}

public func callReferenceParamTemplates() {
  var x: Int = 1
  let _ = templateTypeParamUsedInReferenceParam(&x)
  let _ = templateTypeParamNotUsedInSignatureWithRef(&x, U: Int.self)
}

public func callVarargsTemplates() {
  templateTypeParamNotUsedInSignatureWithVarargs(T: Int.self, U: Int.self)
  // expected-error@-1 {{'templateTypeParamNotUsedInSignatureWithVarargs(T:U:_:)' is unavailable: Variadic function is unavailable}}
  templateTypeParamNotUsedInSignatureWithVarargsAndUnrelatedParam(
  // expected-error@-1 {{'templateTypeParamNotUsedInSignatureWithVarargsAndUnrelatedParam(_:T:U:V:_:)' is unavailable: Variadic function is unavailable}}
    0, T: Int.self, U: Int.self, V: Int.self)
}

// Function templates with non-type template parameters are not imported;
// try to call one anyway to make sure we don't crash trying to resolve it.
public func callNonTypeParamTemplate() {
  templateTypeParamNotUsedInSignatureWithNonTypeParam(T: Int.self)
  // expected-error@-1 {{cannot find 'templateTypeParamNotUsedInSignatureWithNonTypeParam' in scope}}
}

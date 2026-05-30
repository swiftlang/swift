// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default
// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature ImportCxxMembersLazily
//
// REQUIRES: swift_feature_ImportCxxMembersLazily

import TemplateTypeParameterNotInSignature

public func callMemberFunctionTemplates(_ s: Struct) {
  s.templateTypeParamNotUsedInSignature(T: Int.self)
  let _: Int = s.templateTypeParamUsedInReturnType(0)
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

// FIXME: Function templates with varargs are imported but marked unavailable
// (see the corresponding -module-interface test), but the availability
// diagnostic does not seem to fire on this call site.
public func callVarargsTemplates() {
  templateTypeParamNotUsedInSignatureWithVarargs(T: Int.self, U: Int.self)
  templateTypeParamNotUsedInSignatureWithVarargsAndUnrelatedParam(
    0, T: Int.self, U: Int.self, V: Int.self)
}

// Function templates with non-type template parameters are not imported;
// try to call one anyway to make sure we don't crash trying to resolve it.
public func callNonTypeParamTemplate() {
  templateTypeParamNotUsedInSignatureWithNonTypeParam(T: Int.self)
  // expected-error@-1 {{cannot find 'templateTypeParamNotUsedInSignatureWithNonTypeParam' in scope}}
}

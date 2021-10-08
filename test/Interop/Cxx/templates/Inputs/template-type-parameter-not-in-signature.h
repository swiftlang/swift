#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_TEMPLATE_TYPE_PARAMETER_NOT_IN_SIGNATURE_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_TEMPLATE_TYPE_PARAMETER_NOT_IN_SIGNATURE_H

template <typename T>
void templateTypeParamNotUsedInSignature() {}

template <typename T, typename U>
void multiTemplateTypeParamNotUsedInSignature() {}

template <typename T, typename U>
U multiTemplateTypeParamOneUsedInSignature(U u) { return u; }

template <typename T, typename U>
void multiTemplateTypeParamNotUsedInSignatureWithUnrelatedParams(int x, long y) {}

template <typename T>
T templateTypeParamUsedInReturnType(int x) { return x; }

template <typename T>
T templateTypeParamUsedInReferenceParam(T &t) { return t; }

template <typename T, typename U>
void templateTypeParamNotUsedInSignatureWithVarargs(...) {}

template <typename T, typename U, typename V>
void templateTypeParamNotUsedInSignatureWithVarargsAndUnrelatedParam(int x, ...) {}

template <typename T, int N>
void templateTypeParamNotUsedInSignatureWithNonTypeParam() {}

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_TEMPLATE_TYPE_PARAMETER_NOT_IN_SIGNATURE_H

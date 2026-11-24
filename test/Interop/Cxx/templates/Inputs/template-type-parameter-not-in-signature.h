#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_TEMPLATE_TYPE_PARAMETER_NOT_IN_SIGNATURE_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_TEMPLATE_TYPE_PARAMETER_NOT_IN_SIGNATURE_H

struct Struct {
  template <typename T>
  void templateTypeParamNotUsedInSignature() const {}

  template <typename T>
  int templateTypeParamNotUsedInSignatureWithUnnamedParam(int) const {
    return 8;
  }

  template <typename T>
  T templateTypeParamUsedInReturnType(int x) const { return x; }

  template <typename T>
  void templateTypeParamNotUsedInSignatureMutable() {}

  template <typename T>
  static void templateTypeParamNotUsedInSignatureStatic() {}
};

struct StructWithAttributes {
  int field = 0;

  template <typename T>
  const int *templateTypeParamNotUsedInSignatureAddress() const {
    return &field;
  }

  template <typename T>
  int templateTypeParamNotUsedInSignatureResult() const { return 0; }

  template <typename T>
  [[deprecated("use something else")]] void
  templateTypeParamNotUsedInSignatureDeprecated() const {}

  template <typename T>
  // expected-note@+1 {{'templateTypeParamNotUsedInSignatureUnavailable(T:)' has been explicitly marked unavailable here}}
  void templateTypeParamNotUsedInSignatureUnavailable() const
      __attribute__((unavailable("not here")));

  template <typename T>
  void templateTypeParamNotUsedInSignatureUnsafe() const
      __attribute__((swift_attr("unsafe"))) {}

  template <typename T>
  // expected-note@+1 {{calls to instance method 'templateTypeParamNotUsedInSignatureMainActor(T:)' from outside of its actor context are implicitly asynchronous}}
  void templateTypeParamNotUsedInSignatureMainActor() const
      __attribute__((swift_attr("@MainActor"))) {}

  template <typename T>
  T templateTypeParamUsedInReturnTypeUnsafe(int x) const
      __attribute__((swift_attr("unsafe"))) {
    return x;
  }
};

template <typename T>
struct is_bool {
  constexpr static bool value = false;
};

template <>
struct is_bool<bool> {
  constexpr static bool value = true;
};

template <typename T>
bool templateTypeParamNotUsedInSignature() {
  return is_bool<T>::value;
}

template <typename T, typename U>
void multiTemplateTypeParamNotUsedInSignature() {}

template <typename T, typename U>
U multiTemplateTypeParamOneUsedInSignature(U u) { return u; }

template <typename T, typename U>
void multiTemplateTypeParamNotUsedInSignatureWithUnrelatedParams(int x, int y) {}

template <typename T, typename U>
int multiTemplateTypeParamNotUsedInSignatureWithUnnamedParams(int, int) {
  return 7;
}

template <typename T>
T templateTypeParamUsedInReturnType(int x) { return x; }

template <typename T>
T templateTypeParamUsedInReferenceParam(T &t) { return t; }

template <typename T, typename U>
T templateTypeParamNotUsedInSignatureWithRef(T &t) { return t; }

template <typename T, typename U>
// expected-note@+1 {{'templateTypeParamNotUsedInSignatureWithVarargs(T:U:_:)' has been explicitly marked unavailable here}}
void templateTypeParamNotUsedInSignatureWithVarargs(...) {}

template <typename T, typename U, typename V>
// expected-note@+1 {{'templateTypeParamNotUsedInSignatureWithVarargsAndUnrelatedParam(_:T:U:V:_:)' has been explicitly marked unavailable here}}
void templateTypeParamNotUsedInSignatureWithVarargsAndUnrelatedParam(int x, ...) {}

template <typename T, int N>
void templateTypeParamNotUsedInSignatureWithNonTypeParam() {}

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_TEMPLATE_TYPE_PARAMETER_NOT_IN_SIGNATURE_H

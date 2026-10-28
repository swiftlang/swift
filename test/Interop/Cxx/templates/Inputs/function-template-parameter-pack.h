#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_FUNCTION_TEMPLATE_PARAMETER_PACK_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_FUNCTION_TEMPLATE_PARAMETER_PACK_H

// Those below include 'pack'/'Pack' in their names on purpose. We will grep by
// name to ensure they are not imported.

template <typename... Ts>
void takesPack(Ts... ts) {}

template <typename T, typename... Ts>
void takesTypeAndPack(T t, Ts... ts) {}

template <typename... Ts>
void unusedPack() {}

template <typename... Ts>
int packInReturnTypeOnly() {
  return sizeof...(Ts);
}

// The struct should be included, but not its member functions.
struct HasVariadicTemplateMembers {
  template <typename... Ts>
  void memberTakesPack(Ts... ts) {}

  template <typename... Ts>
  static void staticMemberTakesPack(Ts... ts) {}
};

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_FUNCTION_TEMPLATE_PARAMETER_PACK_H

#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_DEPENDENT_TYPES_UNNAMED_PARAM_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_DEPENDENT_TYPES_UNNAMED_PARAM_H

template <typename T>
struct Dependent {
  using type = T;
};

// A dependent parameter type is imported as "Any", so calls go through a
// forwarding thunk that casts the arguments. The unnamed C++ parameter still
// has to be forwarded to the C++ function.
template <typename T>
T dependentUnnamedParam(T a, typename Dependent<T>::type) {
  return a;
}

// A dependent result type builds a thunk too.
template <typename T>
typename Dependent<T>::type dependentResultUnnamedParam(T, T b) {
  return b;
}

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_DEPENDENT_TYPES_UNNAMED_PARAM_H

#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_DEPENDENT_TYPES_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_DEPENDENT_TYPES_H

template<class T>
struct M {
  T value;

  using U = T;

  T getValue() const { return value; }

  template<class U>
  M<U> memberDependentReturnType(U a) const { return {a}; }

  template<class U>
  M<U> memberDependentReturnTypeMutable(U a) { return {a}; }

  template<class U>
  static M<U> memberDependentReturnTypeStatic(U a) { return {a}; }

  template<class U>
  U memberDependentParamType(M<U> a) const { return a.value; }
};

template<class T, class U>
M<U> differentDependentArgAndRet(M<T> a) { return {a.value}; }

template<class T>
M<T> dependantReturnTypeInferred(T a) { return {a}; }

template<class T>
M<T> dependantReturnTypeSameAsArg(M<T> a) { return {a.value}; }

// TODO: still not supported yet (rdar://89034704)
template<class T, class U>
typename M<U>::U complexDifferentDependentArgAndRet(typename M<T>::U  a) { return a; }

template<class T>
typename M<T>::U complexDependantReturnTypeInferred(T a) { return a; }

template<class T>
typename M<T>::U complexDependantReturnTypeSameAsArg(typename M<T>::U  a) { return a; }

template<class T>
M<T> multipleArgs(M<T> a, T b, int c) { return {a.value + b}; }

template<class T, class U>
M<T> multipleDependentArgsInferred(M<T> a, M<U> b, T c, U d) { return {a.value}; }

template<class T, class U>
M<T> multipleDependentArgs(M<T> a, M<U> b) { return {a.value}; }

template<class T>
M<T> refToDependent(T &a) { return {a}; }

template<class T>
M<T> constRefToDependent(const T &a) { return {a}; }

// TODO: We can't import this template rdar://89028943
template<class T>
T &dependentToRef(M<T> a) { return a.value; }

// TODO: We can't convert inout types in a thunk: rdar://89034440
template<class T>
M<T> dependentRef(M<T> &a) { return {a.value}; }

template<class T>
M<T> dependentRefAndRefInferred(const M<T> &a, T &b) { return {a.value + b}; }

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_DEPENDENT_TYPES_H

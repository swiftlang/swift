#pragma once

// Nullability specifiers on dependent types are only checked once the template
// is instantiated. Swift maps them to the optionality of the imported generic
// signature.

// expected-error@+1 {{could not substitute parameters for C++ function template 'nullableResult': optional type 'UnsafeMutablePointer<CInt>?' cannot replace template parameter 'T', which is declared with a nullability specifier}}
template <class T>
T _Nullable nullableResult(T x) {
  return x;
}

template <class T>
T _Nonnull nonnullResult(T x) {
  return x;
}

template <class T>
T _Null_unspecified unspecifiedResult(T x) {
  return x;
}

template <class T>
T _Nullable nullableParameter(T _Nullable x) {
  return x;
}

template <class T>
T *_Nullable nullablePointerResult(T *_Nonnull x) {
  return x;
}

template <class T>
const T *_Nullable nullableConstPointerResult(const T *_Nonnull x) {
  return x;
}

template <class T>
T *_Nonnull nonnullPointerResult(T *_Nonnull x) {
  return x;
}

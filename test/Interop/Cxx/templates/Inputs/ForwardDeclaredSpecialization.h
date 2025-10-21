#ifndef FORWARD_DECLARED_SPECIALIZATION_H
#define FORWARD_DECLARED_SPECIALIZATION_H

// Basic template definition
template <typename T>
struct BasicTemplate {
  T value;
};

// Case 1: Forward-declared specialization (should NOT import)
template <>
struct BasicTemplate<int>;
typedef BasicTemplate<int> ForwardDeclaredInt;

// Case 2: Complete specialization (should import successfully)
template <>
struct BasicTemplate<double> {
  double value;
  double getValue() const { return value; }
};
typedef BasicTemplate<double> CompleteDouble;

// Case 3: Specialization defined after typedef (should import)
template <>
struct BasicTemplate<float>;
typedef BasicTemplate<float> FloatTypedef;

template <>
struct BasicTemplate<float> {
  float value;
};

// Case 4: For comparison - forward-declared non-templated struct (have same behavior)
struct ForwardDeclaredStruct;
typedef ForwardDeclaredStruct ForwardDeclaredStructType;

// Case 5: Complete non-templated struct (imports successfully)
struct CompleteStruct {
  int value;
};
typedef CompleteStruct CompleteStructType;

// Template for partial specialization test
template <typename T, typename U>
struct PartialTemplate {
  T first;
  U second;
};

// Case 6: Forward-declared partial specialization (should NOT import - same as explicit)
template <typename T>
struct PartialTemplate<T*, int>;  // Forward declaration only
typedef PartialTemplate<double*, int> ForwardDeclaredPartial;

// Case 7: Complete partial specialization (should import successfully)
template <typename T>
struct PartialTemplate<T*, double> {
  T* ptr;
  double value;
};
typedef PartialTemplate<int*, double> CompletePartial;

#endif

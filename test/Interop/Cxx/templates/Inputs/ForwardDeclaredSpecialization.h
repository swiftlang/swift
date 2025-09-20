#ifndef FORWARD_DECLARED_SPECIALIZATION_H
#define FORWARD_DECLARED_SPECIALIZATION_H

template <typename T> 
struct MyTemplate { 
  T value; 
};

template <> 
struct MyTemplate<int>;
typedef MyTemplate<int> MyIntTemplate;

template <> 
struct MyTemplate<int>;
typedef MyTemplate<int> MyIntTemplate2;

template <> 
struct MyTemplate<double> {
  double value;
};
typedef MyTemplate<double> MyCompleteDoubleTemplate;

typedef MyTemplate<float> MyFloatTemplate;
typedef MyTemplate<bool> MyCompleteBoolTemplate;

template <>
struct MyTemplate<char>;
typedef MyTemplate<char> MyCharTemplate;
template <>
struct MyTemplate<char> {
  char value;
};

typedef MyTemplate<char> MyCharTemplate2;

#endif
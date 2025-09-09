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
struct MyTemplate<double> {
  double value;
};
typedef MyTemplate<double> MyCompleteIntTemplate;

#endif // FORWARD_DECLARED_SPECIALIZATION_H

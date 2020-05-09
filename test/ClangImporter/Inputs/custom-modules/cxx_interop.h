#pragma once

// Ensure c++ features are used.
namespace ns {
class T {};
class NamespacedType {};

T *doMakeT();
} // namespace ns

struct Basic {
  int a;
  ns::T *b;
};

Basic makeA();

ns::T* makeT();
void useT(ns::T* v);

using namespacedT = ns::T;
using ns::NamespacedType;

class Methods {
 public:
  virtual ~Methods();

  int SimpleMethod(int);

  int SimpleConstMethod(int) const;
  int some_value;

  static int SimpleStaticMethod(int);
};

class Methods2 {
public:
  int SimpleMethod(int);
};

enum __attribute__((enum_extensibility(open))) OpenEmptyEnum : char {};

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
  int PrivateSimpleMethod(int) __attribute__((swift_private));

  int SimpleConstMethod(int) const;
  int some_value;
  int private_value __attribute__((swift_private));

  static int SimpleStaticMethod(int);
};

int PrivateFunction() __attribute__((swift_private));

class PrivateClass {} __attribute__((swift_private));

class ClassWithImportedField {
 public:
  int field;
};

class ClassWithUnimportedField {
 public:
  int field __attribute__((swift_private));
};

class Methods2 {
public:
  int SimpleMethod(int);
};

enum __attribute__((enum_extensibility(open))) OpenEmptyEnum : char {};

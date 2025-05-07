#ifndef TEST_INTEROP_CXX_CLASS_INPUTS_SIMPLE_STRUCTS_H
#define TEST_INTEROP_CXX_CLASS_INPUTS_SIMPLE_STRUCTS_H

#include <string>

struct HasPrivateFieldsOnly {
private:
  int privInt;
  std::string privStr;

public:
  HasPrivateFieldsOnly(int i, std::string s) : privInt(i), privStr(s) {}
};

struct HasPublicFieldsOnly {
  int pubInt;
  std::string pubStr;

  HasPublicFieldsOnly(int i, std::string s) : pubInt(i), pubStr(s) {}
};

struct HasIntFieldsOnly {
private:
  int a = 1;

public:
  int b = 2;

private:
  int c = 3;

public:
  int d = 4;
};

struct HasPrivateAndPublicFields {
private:
  std::string privStr;

public:
  int pubInt;

private:
  int privInt;

public:
  std::string pubStr;

  HasPrivateAndPublicFields(int i1, int i2, std::string s1, std::string s2)
      : privStr(s2), pubInt(i2), privInt(i1), pubStr(s1) {}
};

#endif

#ifndef TEST_INTEROP_CXX_CLASS_INPUTS_MUTABLE_MEMBERS_H
#define TEST_INTEROP_CXX_CLASS_INPUTS_MUTABLE_MEMBERS_H

struct HasPublicMutableMember {
  mutable int a = 0;

  int foo() const {
    a++;
    return a;
  }
};

struct HasPrivateMutableMember {
private:
  mutable int a = 0;

public:
  void bar() const { a++; }
};

#endif // TEST_INTEROP_CXX_CLASS_INPUTS_MUTABLE_MEMBERS_H

#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_TYPEDEFS_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_TYPEDEFS_H

class Banana {
public:
  inline int peel() const { return 42; }
};

template<class T>
class Peel {
public:
  Peel(T t): t(t) {}
  T t;
  int doPeel() const {
    return t.peel() + 1;
  }
};

typedef Peel<Banana> PeeledBanana;
typedef Peel<Banana> OtherPeeledBanana;


#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_TYPEDEFS_H

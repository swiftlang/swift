#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_TYPEDEFS_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_TYPEDEFS_H

struct Avocado {
public:
  inline int taste() const { return 48; }
};

struct Banana {
public:
  inline int taste() const { return 24; }
};

struct Cucumber {
public:
  inline int taste() const { return 12; }
};

struct DragonFruit {
public:
  inline int taste() const { return 6; }
};

template<class T>
struct Peel {
public:
  T fruit;
  inline int peeledTaste() const {
    return fruit.taste() + 5;
  }
};

template<>
struct Peel<Cucumber> {
  Cucumber fruit;
  int peeledTaste() const {
    return fruit.taste() + 10;
  }
};

inline Peel<Avocado> forceInstantiatingPeeledAvocado() {
  return Peel<Avocado>();
}

typedef Peel<Avocado> PeeledAvocado;

typedef Peel<Banana> PeeledBanana;
typedef Peel<Banana> OtherPeeledBanana;

typedef Peel<Cucumber> PeeledCucumber;

using PeeledDragonFruit = Peel<DragonFruit>;

template<class A, class B>
struct Add {
  A a;
  B b;
  int add() { return a + b; }
};

template <class B> using AddInt = Add<int, B>;

template<typename>
class NotImportedIncompleteType;

template<>
class NotImportedIncompleteType<int>;

typedef NotImportedIncompleteType<int> NotImported;

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_TYPEDEFS_H

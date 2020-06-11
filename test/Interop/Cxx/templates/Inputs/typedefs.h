#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_TYPEDEFS_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_TYPEDEFS_H

template<class T>
struct Peel {
public:
  T fruit;
  inline int peeledTaste() const {
    return fruit.taste() + 5;
  }
};

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

inline Peel<Avocado> forceInstantiatingPeeledAvocado() {
  return Peel<Avocado>();
}

// Peel<Avocado> ClassTemplateSpecializationDecl has definition because function
// above forced the instantiation. It's members are not instantiated, we need to
// instantiate them in Swift.
typedef Peel<Avocado> PeeledAvocado;

// Peel<Banana> ClassTemplateSpecializationDecl doesn't have definition yet, we
// need to instantiate the specialization and its members in Swift.
typedef Peel<Banana> PeeledBanana;
typedef Peel<Banana> OtherPeeledBanana;

template<>
struct Peel<Cucumber> {
  Cucumber fruit;
  int peeledTaste() const {
    return fruit.taste() + 10;
  }
};

// Swift should choose the specialization above for PeeledCucumber. The
// specialization has already its members instantiated.
typedef Peel<Cucumber> PeeledCucumber;

// Test coverage for alias-declaration.
using PeeledDragonFruit = Peel<DragonFruit>;

template<class A, class B>
struct TwoArgTemplate {
  A a;
  B b;
};

template <class B> using IgnoredPartialTemplateInstantiation = TwoArgTemplate<int, B>;

template<typename>
class NotImportedIncompleteType;

template<>
class NotImportedIncompleteType<int>;

typedef NotImportedIncompleteType<int> NotImported;

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_TYPEDEFS_H

#pragma once

/// A number with a poisoned value.
struct Num {
  constexpr static unsigned DEFAULT() {return 0xcafe; }
  unsigned val;
  Num() : val{DEFAULT()} {}
  Num(unsigned val) : val{val} {}
};

/// A wrapper around an array of numbers with many subscripting overloads.
class Overloaded {
  Num storage[4] = {};
public:

  /// subscript w/ signed integer is read-only
  Num operator[](int x) const { return storage[x]; }

  /// subscript w/ unsigned integer is read-write
  const Num &operator[](unsigned int x) const { return storage[x]; }
  Num &operator[](unsigned int x) { return storage[x]; }

  /// subscript w/ Num is read-write but requires mutability
  Num &operator[](Num i) { return storage[i.val]; }

  /// Some weirdo operator[] overloads thrown into the mix
public:
  float operator[](float f) const { return f; }

  struct Unit { unsigned int u = 0xdeadbeef; };
  Unit &operator[](Unit u) {
    static Unit TheUnit{};
    return TheUnit;
  }

  struct Bogus {};
  bool operator[](Bogus) const { return true; }
  Num &operator[](Bogus) { return storage[0]; }

  struct GetCharPtr {};
  struct GetConstCharPtr {};

  char *operator[](GetCharPtr) const { return nullptr; }
  const char *operator[](GetConstCharPtr) const { return nullptr; }

  char *operator[](char *c) const { return c; }
  const char *operator[](const char *c) const { return c; }

#if __cplusplus >= 202302L
  /// Index types with more or less than one argument
public:
  bool operator[]() const { return true; }
  Num &operator[]() { return storage[0]; }

  float operator[](float x, float y) { return x + y; }
  const Num &operator[](float x, float y) const { return storage[0]; }

  int operator[](int x, int y) const { return x + y; }
  Num &operator[](int x, int y) { return storage[0]; }
#endif

  /// Some index types, returning different kinds of types
public:
  struct GetVal { int index; };
  struct GetPtr { int index; };
  struct GetRef { int index; };
  struct GetPtrRef { int index; };
  struct GetRefVal { int index; };
  struct GetValPtr { int index; };

  /// GetVal, GetPtr, and GetRef return value, pointers, and references
  unsigned operator[](GetVal p) { return storage[p.index].val; }
  unsigned &operator[](GetPtr p) { return storage[p.index].val; }
  unsigned *operator[](GetRef r) { return &storage[r.index].val; }

  /// GetPtrRef returns a ptr vs ref depending on this const-ness
  unsigned *operator[](GetPtrRef a) { return &storage[a.index].val; }
  const unsigned &operator[](GetPtrRef a) const { return storage[a.index].val; }

  /// GetPtrRef returns a ref vs val depending on this const-ness
  unsigned &operator[](GetRefVal a) { return storage[a.index].val; }
  unsigned operator[](GetRefVal a) const { return storage[a.index].val; }

  /// GetValPtr returns a val vs ptr depending on this const-ness
  unsigned operator[](GetValPtr a) { return storage[a.index].val; }
  const unsigned *operator[](GetValPtr a) const { return &storage[a.index].val; }

  /// Deprecated subscript
  struct DeprecatedIndex { int index; };
  __attribute__((deprecated("use GetVal instead")))
  unsigned operator[](DeprecatedIndex d) const { return storage[d.index].val; }
};

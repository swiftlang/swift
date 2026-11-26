#ifndef EMPTY_FIELD_LAYOUT_H
#define EMPTY_FIELD_LAYOUT_H

struct Empty {};

struct BaseEmpty {
  Empty e;
};

struct EmptyThenInt {
  Empty e;
  int i;
};

inline EmptyThenInt makeEmptyThenInt(int n) { return EmptyThenInt{{}, n}; }

struct IntThenEmpty {
  int i;
  Empty e;
};

inline IntThenEmpty makeIntThenEmpty(int n) { return IntThenEmpty{n, {}}; }

struct EmptyEmptyInt {
  Empty e1;
  Empty e2;
  int i;
};

inline EmptyEmptyInt makeEmptyEmptyInt(int n) {
  return EmptyEmptyInt{{}, {}, n};
}

struct IntEmptyInt {
  int a;
  Empty e;
  int b;
};

inline IntEmptyInt makeIntEmptyInt(int a, int b) { return IntEmptyInt{a, {}, b}; }

// Empty-base optimization: empty base contributes 0 bytes, i is at offset 0.
struct DerivedFromEmpty : Empty {
  int i;
};

inline DerivedFromEmpty makeDerivedFromEmpty(int n) {
  DerivedFromEmpty v;
  v.i = n;
  return v;
}

// `[[no_unique_address]]` lets the empty member share another field's address.
// sizeof(NoUniqueAddressEmpty) is typically 4 bytes, i at offset 0.
struct NoUniqueAddressEmpty {
  [[no_unique_address]] Empty e;
  int i;
};

inline NoUniqueAddressEmpty makeNoUniqueAddressEmpty(int n) {
  return NoUniqueAddressEmpty{{}, n};
}

struct IntTrailingNoUniqueAddressEmpty {
  int i;
  [[no_unique_address]] Empty e;
};

inline IntTrailingNoUniqueAddressEmpty
makeIntTrailingNoUniqueAddressEmpty(int n) {
  return IntTrailingNoUniqueAddressEmpty{n, {}};
}

// Multi-register C ABI return
struct IntEmptyIntInt {
  int a;
  Empty e;
  int b;
  int c;
};

inline IntEmptyIntInt makeIntEmptyIntInt(int a, int b, int c) {
  return IntEmptyIntInt{a, {}, b, c};
}

// Address-only: non-trivial destructor forces sret.
// This shouldn't exercise the `getAsLoadableCXXRecord` path
struct EmptyAndIntNonTrivial {
  Empty e;
  int i;
  ~EmptyAndIntNonTrivial() {}
};

inline EmptyAndIntNonTrivial makeEmptyAndIntNonTrivial(int n) {
  EmptyAndIntNonTrivial v;
  v.i = n;
  return v;
}

#endif

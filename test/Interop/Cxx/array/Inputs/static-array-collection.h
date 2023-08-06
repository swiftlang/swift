#ifndef TEST_INTEROP_CXX_ARRAY_INPUTS_STATIC_ARRAY_COLLECTION_H
#define TEST_INTEROP_CXX_ARRAY_INPUTS_STATIC_ARRAY_COLLECTION_H

struct HasIntArray {
  int array[8];

  HasIntArray() {
    for (int i = 1; i < 9; ++i) {
      array[i - 1] = i;
    }
  }
};

struct HugeArray {
  int array[64 * 1000];

  HugeArray() {
    array[0] = 0;
    array[42] = 42;
    array[63000] = 42;
  }
};

struct NonTrivial {
  int value = 42;
  NonTrivial(const NonTrivial &) {}
};

struct NonTrivialArray {
  NonTrivial array[8];
};

#endif // TEST_INTEROP_CXX_ARRAY_INPUTS_STATIC_ARRAY_COLLECTION_H

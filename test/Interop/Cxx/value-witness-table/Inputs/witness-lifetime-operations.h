#ifndef TEST_INTEROP_CXX_VALUE_WITNESS_TABLE_INPUTS_WITNESS_LIFETIME_OPERATIONS_H
#define TEST_INTEROP_CXX_VALUE_WITNESS_TABLE_INPUTS_WITNESS_LIFETIME_OPERATIONS_H

struct __attribute__((swift_attr("import_unsafe"))) NonTrivial {
  NonTrivial() { }
  NonTrivial(const NonTrivial& other) { }
  ~NonTrivial() { }
};

#endif // TEST_INTEROP_CXX_VALUE_WITNESS_TABLE_INPUTS_WITNESS_LIFETIME_OPERATIONS_H

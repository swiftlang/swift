#ifndef TEST_INTEROP_CXX_ENUM_INPUTS_BOOL_ENUMS_H
#define TEST_INTEROP_CXX_ENUM_INPUTS_BOOL_ENUMS_H

enum Maybe : bool { No, Yes };
enum BinaryNumbers : bool { One = 1, Zero = 0 };
enum class EnumClass : bool { Foo, Bar };
struct WrapperStruct {
  enum InnerBoolEnum : bool { A, B };
};

#endif // TEST_INTEROP_CXX_ENUM_INPUTS_BOOL_ENUMS_H

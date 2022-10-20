#ifndef TEST_INTEROP_CXX_ENUM_INPUTS_TYPED_UNTYPED_ENUMS_H
#define TEST_INTEROP_CXX_ENUM_INPUTS_TYPED_UNTYPED_ENUMS_H


// Typed enum.
enum Color { kRed = 0, kBlue, kGreen, kYellow = 10 };

// Untyped enum.
enum { kOne = 1, kTwo, kThree, kFour };

// enum class.
enum class Pet { goat = 5, cat = 15, dogcow, rabbit };


#endif // TEST_INTEROP_CXX_ENUM_INPUTS_TYPED_UNTYPED_ENUMS_H
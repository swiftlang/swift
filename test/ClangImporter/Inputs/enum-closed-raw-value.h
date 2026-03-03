#ifndef ENUM_CLOSED_RAWVALUE_H
#define ENUM_CLOSED_RAWVALUE_H

// Test NS_CLOSED_ENUM with explicit enum_extensibility(closed) attribute
typedef enum __attribute__((enum_extensibility(closed))) IntEnum : long {
  IntEnumZero = 0,
  IntEnumOne = 1
} IntEnum;

// Test NS_CLOSED_ENUM with non-contiguous values
typedef enum __attribute__((enum_extensibility(closed))) NonContiguousEnum : long {
  NonContiguousEnumFirst = 10,
  NonContiguousEnumSecond = 20,
  NonContiguousEnumThird = 30
} NonContiguousEnum;

// Test NS_CLOSED_ENUM with negative values
typedef enum __attribute__((enum_extensibility(closed))) NegativeEnum : long {
  NegativeEnumNegative = -1,
  NegativeEnumZero = 0,
  NegativeEnumPositive = 1
} NegativeEnum;

// Test regular NS_ENUM (non-frozen) for comparison - should accept any value
typedef enum __attribute__((enum_extensibility(open))) OpenEnum : long {
  OpenEnumZero = 0,
  OpenEnumOne = 1
} OpenEnum;

// Test NS_CLOSED_ENUM with single case
typedef enum __attribute__((enum_extensibility(closed))) SingleCaseEnum : long {
  SingleCaseEnumOnly = 42
} SingleCaseEnum;

#endif // ENUM_CLOSED_RAWVALUE_H

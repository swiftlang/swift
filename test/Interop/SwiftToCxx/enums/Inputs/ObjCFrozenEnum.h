#pragma once

#if __has_attribute(enum_extensibility)
#define CLOSED_ENUM_ATTR __attribute__((enum_extensibility(closed)))
#else
#define CLOSED_ENUM_ATTR
#endif

typedef enum CLOSED_ENUM_ATTR ObjCFrozenEnum : unsigned int {
    ObjCFrozenEnumFirst = 0,
    ObjCFrozenEnumSecond = 1,
    ObjCFrozenEnumThird = 2,
} ObjCFrozenEnum;

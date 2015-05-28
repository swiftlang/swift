@import Foundation;

#define MAKE_ENUM(name) \
  typedef NS_ENUM(NSInteger, name) { \
    name##WaterMelon,                \
    name##Orange                     \
  }                                  \

MAKE_ENUM(MyCoolEnum);


typedef NS_ENUM(NSInteger, ALL_CAPS_ENUM) {
  ENUM_CASE_ONE,
  ENUM_CASE_TWO
};
typedef NS_ENUM(NSInteger, ALL_CAPS_ENUM2) {
  ALL_CAPS_CASE_ONE,
  ALL_CAPS_CASE_TWO
};

typedef NS_ENUM(NSInteger, SomeRandomEnum) {
  SomeRandomA,
  SomeRandomB
};

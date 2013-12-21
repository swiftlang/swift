@import Foundation;

#define MAKE_ENUM(name) \
  typedef NS_ENUM(NSInteger, name) { \
    name##WaterMelon,                \
    name##Orange                     \
  }                                  \

MAKE_ENUM(MyCoolEnum);

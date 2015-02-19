@import Foundation;

enum : int8_t {
  NamelessInt8Constant = 1
};

enum : uint32_t {
  NamelessInt32Constant = 0x10
};

enum : int64_t {
  NamelessInt64Constant = 0x100
};

enum OpaqueInt8 : int8_t {
  OpaqueInt8Constant = 2
};

enum OpaqueUInt32 : uint32_t {
  OpaqueInt32Constant = 0x20
};

enum OpaqueInt64 : int64_t {
  OpaqueInt64Constant = 0x200
};

typedef NS_ENUM(int8_t, EnumInt8) {
  EnumInt8Constant = 4
};

typedef NS_ENUM(uint32_t, EnumInt32) {
  EnumInt32Constant = 0x40
};

typedef NS_ENUM(int64_t, EnumInt64) {
  EnumInt64Constant = 0x400
};

typedef NS_OPTIONS(int8_t, OptionsInt8) {
  OptionsInt8Constant = 8
};

typedef NS_OPTIONS(int64_t, OptionsInt32) {
  OptionsInt32Constant = 0x80
};

typedef NS_OPTIONS(int64_t, OptionsInt64) {
  OptionsInt64Constant = 0x800
};

#define INCLUDE_ALL(Prefix) \
  Prefix##Nameless8 = NamelessInt8Constant, \
  Prefix##Nameless32 = NamelessInt32Constant, \
  Prefix##Nameless64 = NamelessInt64Constant, \
  Prefix##Opaque8 = OpaqueInt8Constant, \
  Prefix##Opaque32 = OpaqueInt32Constant, \
  Prefix##Opaque64 = OpaqueInt64Constant, \
  Prefix##Enum8 = EnumInt8Constant, \
  Prefix##Enum32 = EnumInt32Constant, \
  Prefix##Enum64 = EnumInt64Constant, \
  Prefix##Options8 = OptionsInt8Constant, \
  Prefix##Options32 = OptionsInt32Constant, \
  Prefix##Options64 = OptionsInt64Constant \

enum : uint32_t { INCLUDE_ALL(Nameless) };
enum OpaqueDerived : uint32_t { INCLUDE_ALL(OpaqueDerived) };
typedef NS_ENUM(uint32_t, EnumDerived) { INCLUDE_ALL(EnumDerived) };
typedef NS_OPTIONS(uint32_t, OptionsDerived) { INCLUDE_ALL(OptionsDerived) };

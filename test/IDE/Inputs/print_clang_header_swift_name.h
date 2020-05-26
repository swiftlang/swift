@import Foundation;

#ifndef SWIFT_ENUM_EXTRA
# define SWIFT_ENUM_EXTRA
#endif

#define SWIFT_COMPILE_NAME(X) __attribute__((swift_name(X)))

#define SWIFT_ENUM(_type, _name) enum _name : _type _name; enum __attribute__((enum_extensibility(open))) SWIFT_ENUM_EXTRA _name : _type

#pragma clang attribute push( \
  __attribute__((external_source_symbol(language="Swift", \
                 defined_in="Mixed",generated_declaration))), \
  apply_to=any(function,enum,objc_interface,objc_category,objc_protocol))

typedef SWIFT_ENUM(NSInteger, Normal) {
    NormalOne = 0,
    NormalTwo,
    NormalThree
};

#define SWIFT_ENUM_NAMED(_type, _name, _swiftName) enum _name : _type _name SWIFT_COMPILE_NAME(_swiftName); enum SWIFT_COMPILE_NAME(_swiftName) __attribute__((enum_extensibility(open))) SWIFT_ENUM_EXTRA _name : _type

typedef SWIFT_ENUM_NAMED(NSInteger, ObjCEnum, "SwiftEnum") {
    ObjCEnumOne = 0,
    ObjCEnumTwo,
    ObjCEnumThree
};

typedef SWIFT_ENUM_NAMED(NSInteger, ObjCEnumTwo, "SwiftEnumTwo") {
    // the following shouldn't have their prefixes stripped
    SwiftEnumTwoA,
    SwiftEnumTwoB,
    SwiftEnumTwoC
};

#pragma clang attribute pop

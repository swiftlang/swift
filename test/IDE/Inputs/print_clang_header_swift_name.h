@import Foundation;

#define SWIFT_COMPILE_NAME(X) __attribute__((swift_name(X)))

#define SWIFT_ENUM(_type, _name) enum _name : _type _name; enum __attribute__((enum_extensibility(open))) SWIFT_ENUM_EXTRA _name : _type

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

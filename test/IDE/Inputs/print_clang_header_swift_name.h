@import Foundation;

#define SWIFT_COMPILE_NAME(X) __attribute__((swift_name(X)))

#define SWIFT_ENUM(_type, _name) enum _name : _type _name; enum SWIFT_ENUM_EXTRA _name : _type

typedef SWIFT_ENUM(NSInteger, Normal) {
    NormalOne = 0,
    NormalTwo,
    NormalThree
};

// FIXME (#618): Use SWIFT_ENUM_NAMED() when support for that lands
#undef SWIFT_ENUM
#define SWIFT_ENUM(_type, _name) enum _name : _type _name SWIFT_COMPILE_NAME(SWIFT_ENUM_NAME); enum SWIFT_COMPILE_NAME(SWIFT_ENUM_NAME) SWIFT_ENUM_EXTRA _name : _type

#define SWIFT_ENUM_NAME "SwiftEnum"
typedef SWIFT_ENUM(NSInteger, ObjCEnum) {
    ObjCEnumOne = 0,
    ObjCEnumTwo,
    ObjCEnumThree
};

#undef SWIFT_ENUM_NAME
#define SWIFT_ENUM_NAME "SwiftEnumTwo"
typedef SWIFT_ENUM(NSInteger, ObjCEnumTwo) {
    // the following shouldn't have their prefixes stripped
    SwiftEnumTwoA,
    SwiftEnumTwoB,
    SwiftEnumTwoC
};

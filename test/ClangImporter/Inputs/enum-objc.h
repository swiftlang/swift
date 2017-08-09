@import Foundation;

#define SWIFT_COMPILE_NAME(X) __attribute__((swift_name(X)))
#define SWIFT_ENUM_NAMED(_type, _name, SWIFT_NAME) enum _name : _type _name SWIFT_COMPILE_NAME(SWIFT_NAME); enum SWIFT_COMPILE_NAME(SWIFT_NAME) __attribute__((enum_extensibility(open))) SWIFT_ENUM_EXTRA _name : _type

typedef SWIFT_ENUM_NAMED(NSInteger, ObjCEnum, "SwiftEnum") {
    ObjCEnumOne = 1,
    ObjCEnumTwo,
    ObjCEnumThree
};

enum BareForwardEnum;
enum ForwardEnumWithUnderlyingType : int;
typedef SWIFT_ENUM_NAMED(NSInteger, ForwardObjCEnum, "ForwardSwiftEnum");

void forwardBarePointer(enum BareForwardEnum * _Nonnull);
void forwardWithUnderlyingValue(enum ForwardEnumWithUnderlyingType);
void forwardWithUnderlyingPointer(enum ForwardEnumWithUnderlyingType * _Nonnull);
void forwardObjCValue(ForwardObjCEnum);
void forwardObjCPointer(ForwardObjCEnum * _Nonnull);

@import Foundation;

#ifndef SWIFT_ENUM_EXTRA
# define SWIFT_ENUM_EXTRA
#endif

#define SWIFT_COMPILE_NAME(X) __attribute__((swift_name(X)))
#define SWIFT_ENUM_NAMED(_type, _name, SWIFT_NAME) enum _name : _type _name SWIFT_COMPILE_NAME(SWIFT_NAME); enum SWIFT_COMPILE_NAME(SWIFT_NAME) __attribute__((enum_extensibility(open))) SWIFT_ENUM_EXTRA _name : _type
#define SWIFT_EXHAUSTIVE_ENUM(_type, _name) enum _name : _type _name; enum __attribute__((enum_extensibility(closed))) SWIFT_ENUM_EXTRA _name : _type

#pragma clang attribute push( \
  __attribute__((external_source_symbol(language="Swift", \
                 defined_in="Mixed",generated_declaration))), \
  apply_to=any(function,enum,objc_interface,objc_category,objc_protocol))

typedef SWIFT_ENUM_NAMED(NSInteger, ObjCEnum, "SwiftEnum") {
    ObjCEnumOne = 1,
    ObjCEnumTwo,
    ObjCEnumThree
};

typedef SWIFT_EXHAUSTIVE_ENUM(NSInteger, ExhaustiveEnum) {
    ExhaustiveEnumOne = 1,
    ExhaustiveEnumTwo,
    ExhaustiveEnumThree
};

enum BareForwardEnum;
enum ForwardEnumWithUnderlyingType : int;
typedef SWIFT_ENUM_NAMED(NSInteger, ForwardObjCEnum, "ForwardSwiftEnum");

#pragma clang attribute pop

void forwardBarePointer(enum BareForwardEnum * _Nonnull);
void forwardWithUnderlyingValue(enum ForwardEnumWithUnderlyingType);
void forwardWithUnderlyingPointer(enum ForwardEnumWithUnderlyingType * _Nonnull);
void forwardObjCValue(ForwardObjCEnum);
void forwardObjCPointer(ForwardObjCEnum * _Nonnull);

@interface SomeClass : NSObject
+ (void)tryInferDefaultArgumentUnderlyingValue:(bool)dummy options:(enum ForwardEnumWithUnderlyingType)options;
+ (void)tryInferDefaultArgumentObjCValue:(bool)dummy options:(ForwardObjCEnum)options;
@end

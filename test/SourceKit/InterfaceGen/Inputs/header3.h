#define CF_ENUM(_type, _name) enum _name : _type _name; enum _name : _type
#define CF_OPTIONS(_type, _name) enum _name : _type _name; enum _name : _type
#define NS_ENUM(_type, _name) CF_ENUM(_type, _name)
#define NS_OPTIONS(_type, _name) CF_OPTIONS(_type, _name)
#define CF_SWIFT_NAME(_name) __attribute__((swift_name(#_name)))
#define NS_SWIFT_NAME(_name) CF_SWIFT_NAME(_name)

typedef NS_ENUM(unsigned, SKFuelKind) { SKFuelKindH2, SKFuelKindCH4, SKFuelKindC12H26 };
unsigned SKFuelKindIsCryogenic(SKFuelKind kind) NS_SWIFT_NAME(getter:SKFuelKind.isCryogenic(self:));
unsigned SKFuelKindIsNotCryogenic(SKFuelKind kind) NS_SWIFT_NAME(getter:SKFuelKind.isNotCryogenic(self:));

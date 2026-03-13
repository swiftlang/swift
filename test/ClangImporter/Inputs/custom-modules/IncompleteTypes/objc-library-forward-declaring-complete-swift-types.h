@class Foo;
@class Baz;
@class ConflictingTypeName;
@class subscript;
@class ShadowedProtocol;

void takeAFoo(Foo *foo);
Foo *returnAFoo();

void takeABaz(Baz *baz);
Baz *returnABaz();

void takeAConflictingTypeName(ConflictingTypeName *param);
ConflictingTypeName *returnAConflictingTypeName();

void takeASubscript(subscript *param);
subscript* returnASubscript();

// There is a Swift protocol, @objc protocol ShadowedProtocol, but
// since here we are refering to a class and not a protocol, this
// shouldn't be an issue.
ShadowedProtocol* returnANativeObjCClassShadowedProtocol();

@protocol ProtocolFoo;
@protocol ProtocolBaz;
@protocol ProtocolConflictingTypeName;

id<ProtocolFoo> returnAProtocolFoo();
id<ProtocolBaz> returnAProtocolBaz();
id<ProtocolConflictingTypeName> returnAProtocolConflictingTypeName();

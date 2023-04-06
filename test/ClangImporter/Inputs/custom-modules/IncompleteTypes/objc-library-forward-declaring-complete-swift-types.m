#import "objc-library-forward-declaring-complete-swift-types.h"
#import "CompleteSwiftTypes-Swift.h"

void takeAFoo(Foo *foo) { [foo sayHello]; }

Foo *returnAFoo() {
  Foo *result = [[Foo alloc] init];
  [result sayHello];
  return result;
}

void takeABaz(Baz *baz) { [baz sayHello]; }

Baz *returnABaz() {
  Baz *result = [[Baz alloc] init];
  [result sayHello];
  return result;
}

void takeAConflictingTypeName(ConflictingTypeName *param) { [param sayHello]; }

ConflictingTypeName *returnAConflictingTypeName() {
  ConflictingTypeName *result = [[ConflictingTypeName alloc] init];
  [result sayHello];
  return result;
}

void takeASubscript(subscript *baz) { [baz sayHello]; }

subscript *returnASubscript() {
  subscript *result = [[subscript alloc] init];
  [result sayHello];
  return result;
}

@interface ShadowedProtocol : NSObject
@end

@implementation ShadowedProtocol
@end

ShadowedProtocol* returnANativeObjCClassShadowedProtocol() {
    return [[ShadowedProtocol alloc] init];
}

id<ProtocolFoo> returnAProtocolFoo() {
    return [[ProtocolConformer alloc] init];
}

id<ProtocolBaz> returnAProtocolBaz() {
    return [[ProtocolConformer alloc] init];
}

id<ProtocolConflictingTypeName> returnAProtocolConflictingTypeName() {
    return [[ProtocolConformer alloc] init];
}

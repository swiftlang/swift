#import "incomplete-noroottype-protocol-library.h"

@protocol NoRootTypeProtocol
- (void)sayHello;
@end

@interface NoRootTypeProtocolConformingType : NSObject <NoRootTypeProtocol>
- (void)sayHello;
@end

@implementation NoRootTypeProtocolConformingType
- (void)sayHello {
  NSLog(@"Hello from NoRootTypeProtocolConformingType!");
}
@end

@implementation NoRootTypeProtocolConsumer
- (id)init {
  self = [super init];
  if (self) {
    self.propertyUsingAForwardDeclaredNoRootTypeProtocol =
        [[NoRootTypeProtocolConformingType alloc] init];
  }
  return self;
}
- (id<NoRootTypeProtocol>)methodReturningForwardDeclaredNoRootTypeProtocol {
  NSLog(@"methodReturningForwardDeclaredNoRootTypeProtocol");
  NoRootTypeProtocolConformingType *result =
      [[NoRootTypeProtocolConformingType alloc] init];
  [result sayHello];
  return result;
}
- (void)methodTakingAForwardDeclaredNoRootTypeProtocol:
    (id<NoRootTypeProtocol>)param {
  NSLog(@"methodTakingAForwardDeclaredNoRootTypeProtocol");
  [param sayHello];
}
@end

id<NoRootTypeProtocol> CFunctionReturningAForwardDeclaredNoRootTypeProtocol() {
  NSLog(@"CFunctionReturningAForwardDeclaredNoRootTypeProtocol");
  NoRootTypeProtocolConformingType *result =
      [[NoRootTypeProtocolConformingType alloc] init];
  [result sayHello];
  return result;
}

void CFunctionTakingAForwardDeclaredNoRootTypeProtocol(
    id<NoRootTypeProtocol> param) {
  NSLog(@"CFunctionTakingAForwardDeclaredNoRootTypeProtocol");
  [param sayHello];
}

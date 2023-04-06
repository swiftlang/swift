#import "incomplete-nsproxy-library.h"

@interface ForwardDeclaredNSProxyInterface : NSProxy
- (id)init;
- (void)doSomethingForwardDeclaredNSProxyInterfacesCan;
@end

@implementation ForwardDeclaredNSProxyInterface
- (id)init {
  return self;
}
- (void)doSomethingForwardDeclaredNSProxyInterfacesCan {
  NSLog(@"Doing something forward declared NSProxy can version!");
}
@end

@implementation NSProxyConsumer
- (id)init {
  self = [super init];
  if (self) {
    self.propertyUsingAForwardDeclaredNSProxyInterface =
        [[ForwardDeclaredNSProxyInterface alloc] init];
  }
  return self;
}
- (ForwardDeclaredNSProxyInterface *)
    methodReturningForwardDeclaredNSProxyInterface {
  NSLog(@"methodReturningForwardDeclaredNSProxyInterface");
  return [[ForwardDeclaredNSProxyInterface alloc] init];
}
- (void)methodTakingAForwardDeclaredNSProxyInterface:
    (ForwardDeclaredNSProxyInterface *)param {
  [param doSomethingForwardDeclaredNSProxyInterfacesCan];
}
@end

ForwardDeclaredNSProxyInterface *
CFunctionReturningAForwardDeclaredNSProxyInterface() {
  NSLog(@"CFunctionReturningAForwardDeclaredNSProxyInterface");
  return [[ForwardDeclaredNSProxyInterface alloc] init];
}
void CFunctionTakingAForwardDeclaredNSProxyInterface(
    ForwardDeclaredNSProxyInterface *param) {
  NSLog(@"CFunctionTakingAForwardDeclaredNSProxyInterface");
  [param doSomethingForwardDeclaredNSProxyInterfacesCan];
}

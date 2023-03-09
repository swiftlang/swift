#import "incomplete-type-library-1.h"
#import "complete-types.h"

@interface TypeConformingToForwardDeclaredProtocol1
    : NSObject <ForwardDeclaredProtocol>
- (id)init;
- (void)doSomethingForwardDeclaredProtocolsCan;
@end

@implementation TypeConformingToForwardDeclaredProtocol1
- (id)init {
  return [super init];
}
- (void)doSomethingForwardDeclaredProtocolsCan {
  NSLog(@"Doing something forward declared protocols can version 1!");
}
@end

@implementation IncompleteTypeConsumer1
- (id)init {
  self = [super init];
  if (self) {
    self.propertyUsingAForwardDeclaredInterface1 =
        [[ForwardDeclaredInterface alloc] init];
    self.propertyUsingAForwardDeclaredProtocol1 =
        [[TypeConformingToForwardDeclaredProtocol1 alloc] init];
  }
  return self;
}
- (NSObject<ForwardDeclaredProtocol> *)methodReturningForwardDeclaredProtocol1 {
  NSLog(@"methodReturningForwardDeclaredProtocol1");
  return [[TypeConformingToForwardDeclaredProtocol1 alloc] init];
}
- (ForwardDeclaredInterface *)methodReturningForwardDeclaredInterface1 {
  NSLog(@"methodReturningForwardDeclaredInterface1");
  return [[ForwardDeclaredInterface alloc] init];
}
- (void)methodTakingAForwardDeclaredProtocol1:
    (id<ForwardDeclaredProtocol>)param {
  NSLog(@"methodTakingAForwardDeclaredProtocol1");
  [param doSomethingForwardDeclaredProtocolsCan];
}
- (void)methodTakingAForwardDeclaredInterface1:
    (ForwardDeclaredInterface *)param {
  NSLog(@"methodTakingAForwardDeclaredInterface1");
  [param doSomethingForwardDeclaredInterfacesCan];
}
@end

ForwardDeclaredInterface *CFunctionReturningAForwardDeclaredInterface1() {
  NSLog(@"CFunctionReturningAForwardDeclaredInterface1");
  return [[ForwardDeclaredInterface alloc] init];
}
void CFunctionTakingAForwardDeclaredInterface1(
    ForwardDeclaredInterface *param) {
  NSLog(@"CFunctionTakingAForwardDeclaredInterface1");
  [param doSomethingForwardDeclaredInterfacesCan];
}

NSObject<ForwardDeclaredProtocol> *
CFunctionReturningAForwardDeclaredProtocol1() {
  NSLog(@"CFunctionReturningAForwardDeclaredProtocol1");
  return [[TypeConformingToForwardDeclaredProtocol1 alloc] init];
}
void CFunctionTakingAForwardDeclaredProtocol1(
    id<ForwardDeclaredProtocol> param) {
  NSLog(@"CFunctionTakingAForwardDeclaredProtocol1");
  [param doSomethingForwardDeclaredProtocolsCan];
}

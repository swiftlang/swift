#import "incomplete-type-library-2.h"
#import "complete-types.h"

@interface TypeConformingToForwardDeclaredProtocol2
    : NSObject <ForwardDeclaredProtocol>
- (id)init;
- (void)doSomethingForwardDeclaredProtocolsCan;
@end

@implementation TypeConformingToForwardDeclaredProtocol2
- (id)init {
  return [super init];
}
- (void)doSomethingForwardDeclaredProtocolsCan {
  NSLog(@"Doing something forward declared protocols can version 2!");
}
@end

@implementation IncompleteTypeConsumer2
- (id)init {
  self = [super init];
  if (self) {
    self.propertyUsingAForwardDeclaredInterface2 =
        [[ForwardDeclaredInterface alloc] init];
    self.propertyUsingAForwardDeclaredProtocol2 =
        [[TypeConformingToForwardDeclaredProtocol2 alloc] init];
  }
  return self;
}
- (NSObject<ForwardDeclaredProtocol> *)methodReturningForwardDeclaredProtocol2 {
  NSLog(@"methodReturningForwardDeclaredProtocol2");
  return [[TypeConformingToForwardDeclaredProtocol2 alloc] init];
}
- (ForwardDeclaredInterface *)methodReturningForwardDeclaredInterface2 {
  NSLog(@"methodReturningForwardDeclaredInterface2");
  return [[ForwardDeclaredInterface alloc] init];
}
- (void)methodTakingAForwardDeclaredProtocol2:
    (id<ForwardDeclaredProtocol>)param {
  NSLog(@"methodTakingAForwardDeclaredProtocol2");
  [param doSomethingForwardDeclaredProtocolsCan];
}
- (void)methodTakingAForwardDeclaredInterface2:
    (ForwardDeclaredInterface *)param {
  NSLog(@"methodTakingAForwardDeclaredInterface2");
  [param doSomethingForwardDeclaredInterfacesCan];
}
@end

ForwardDeclaredInterface *CFunctionReturningAForwardDeclaredInterface2() {
  NSLog(@"CFunctionReturningAForwardDeclaredInterface2");
  return [[ForwardDeclaredInterface alloc] init];
}
void CFunctionTakingAForwardDeclaredInterface2(
    ForwardDeclaredInterface *param) {
  NSLog(@"CFunctionTakingAForwardDeclaredInterface2");
  [param doSomethingForwardDeclaredInterfacesCan];
}

NSObject<ForwardDeclaredProtocol> *
CFunctionReturningAForwardDeclaredProtocol2() {
  NSLog(@"CFunctionReturningAForwardDeclaredProtocol2");
  return [[TypeConformingToForwardDeclaredProtocol2 alloc] init];
}
void CFunctionTakingAForwardDeclaredProtocol2(
    id<ForwardDeclaredProtocol> param) {
  NSLog(@"CFunctionTakingAForwardDeclaredProtocol2");
  [param doSomethingForwardDeclaredProtocolsCan];
}

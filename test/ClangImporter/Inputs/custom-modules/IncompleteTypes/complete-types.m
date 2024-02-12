#import "complete-types.h"

@implementation ForwardDeclaredInterface
- (id)init {
  return [super init];
}
- (void)doSomethingForwardDeclaredInterfacesCan {
  NSLog(@"Doing something forward declared interfaces can!");
}
@end

void takeACompleteInterface(ForwardDeclaredInterface *param) {
  NSLog(@"takeACompleteInterface");
  [param doSomethingForwardDeclaredInterfacesCan];
}
void takeACompleteProtocol(NSObject<ForwardDeclaredProtocol> *param) {
  NSLog(@"takeACompleteProcotol");
  [param doSomethingForwardDeclaredProtocolsCan];
}

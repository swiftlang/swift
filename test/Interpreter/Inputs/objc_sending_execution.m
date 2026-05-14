
#import "objc_sending_execution.h"

@implementation Foo {
  __weak id <FooDelegate> _delegate;
}

@synthesize delegate=_delegate;

- (NSString *)doSomething {
  NSObject *obj = [[NSObject alloc] init];
  obj = [_delegate identityFn: obj];
  return [_delegate takingObject: obj];
}

@end


// ----------------------------------------------------
// From https://github.com/swiftlang/swift/issues/87659

@implementation Payload
@end

@implementation LegacyObjCHandler
- (void)handle:(Payload *)value {
    NSLog(@"LegacyObjCHandler received: %@", value.name);
}
@end

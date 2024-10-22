#import "availability_non_runtime_protocol_objc_protocol.h"
// ObjC defines a protocol (ComposerMutationBridging) and Swift will use the
// protocol. Here, we import Swift class (Wrapper) to ObjC.
#import "SwiftCallObjC-Swift.h"

@interface Foo : NSObject <ComposerMutationBridging>
@end
@implementation Foo
@end

@implementation CustomObject
- (void)someMethod {
  NSLog(@"SomeMethod Ran");
  Foo *f = [[Foo alloc] init];
  NSArray<id<ComposerMutationBridging>> *persons =
      [NSArray arrayWithObjects:f, nil];
  Wrapper *w = [[Wrapper alloc] init];
  // This Swift method takes the input of [ComposerMutationBridging].
  [w runMutations:persons];

  NSLog(NSStringFromProtocol(@protocol(ObjCNonRuntimeProtocolNotUsedinSwift)));
}
@end

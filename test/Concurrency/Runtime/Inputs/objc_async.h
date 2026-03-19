#include <Foundation/Foundation.h>

@interface MyNSInterfaceWithCallbackFunc: NSObject

- (instancetype _Nonnull)init;

- (void)compute:(NSInteger)x completionHandler:(void (^ _Nonnull)(NSInteger))handler;

@end

@interface MutableMyNSInterfaceWithCallbackFunc: MyNSInterfaceWithCallbackFunc
@end

@interface MutableMyNSInterfaceWithCallbackFunc_v2: MutableMyNSInterfaceWithCallbackFunc
@end

@interface Farm: NSObject

-(void)getDogWithCompletion:(void (^ _Nonnull)(NSInteger))completionHandler
  __attribute__((swift_async_name("getter:doggo()")));

-(void)obtainCat:(void (^ _Nonnull)(NSInteger, NSError* _Nullable))completionHandler
__attribute__((swift_async_name("getter:catto()")));

@end

void scheduleCallback(MyNSInterfaceWithCallbackFunc * _Nonnull b, NSString * _Nonnull s);

/// Call [b compute:x completionHandler:] and block until the handler fires.
/// Returns the result delivered to the handler.
NSInteger callComputeAndWaitSemaphore(MyNSInterfaceWithCallbackFunc * _Nonnull b, NSInteger x);

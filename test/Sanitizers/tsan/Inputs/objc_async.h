#include <Foundation/Foundation.h>

@interface MyNSInterfaceWithCallbackFunc: NSObject

- (instancetype _Nonnull)init;

- (void)compute:(NSInteger)x completionHandler:(void (^ _Nonnull)(NSInteger))handler;

@end

#include <Foundation/Foundation.h>

@interface Butt: NSObject

- (instancetype _Nonnull)init;

- (void)butt:(NSInteger)x completionHandler:(void (^ _Nonnull)(NSInteger))handler;

@end

#include "objc_async.h"
#include <stdio.h>

@implementation MyNSInterfaceWithCallbackFunc

- (instancetype)init {
  return [super init];
}

- (void)compute:(NSInteger)x completionHandler:(void (^)(NSInteger))handler {
  printf("starting %ld\n", (long)x);
  handler(679);
}

@end

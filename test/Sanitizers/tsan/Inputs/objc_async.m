#include "objc_async.h"
#include <stdio.h>

@implementation Butt

- (instancetype)init {
  return [super init];
}

- (void)butt:(NSInteger)x completionHandler:(void (^)(NSInteger))handler {
  printf("starting %ld\n", (long)x);
  handler(679);
}

@end

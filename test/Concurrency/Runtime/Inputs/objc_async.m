#include "objc_async.h"
#include <stdio.h>
#include <dispatch/dispatch.h> // since these are objc tests, these are on Darwin and we have dispatch

@implementation MyNSInterfaceWithCallbackFunc

- (instancetype)init {
  return [super init];
}

- (void)compute:(NSInteger)x completionHandler:(void (^)(NSInteger))handler {
  printf("starting %ld\n", (long)x);
  handler(679);
}

@end

@implementation MutableMyNSInterfaceWithCallbackFunc: MyNSInterfaceWithCallbackFunc
@end

@implementation MutableMyNSInterfaceWithCallbackFunc_v2: MutableMyNSInterfaceWithCallbackFunc
@end


@implementation Farm

-(void)getDogWithCompletion:(void (^ _Nonnull)(NSInteger))completionHandler {
  printf("getting dog\n");
  completionHandler(123);
}

-(void)obtainCat:(void (^ _Nonnull)(NSInteger, NSError* _Nullable))completionHandler {
  printf("obtaining cat has failed!\n");
  completionHandler(0, [NSError errorWithDomain:@"obtainCat" code:456 userInfo:nil]);
}

@end

void scheduleCallback(MyNSInterfaceWithCallbackFunc *b, NSString *s) {
  [b compute: 1738 completionHandler: ^(NSInteger i) {
    printf("callback %p named %s occurred at %zd\n", b, [s UTF8String], (ssize_t)i);
    fflush(stdout);
  }];
}

NSInteger callComputeAndWaitSemaphore(MyNSInterfaceWithCallbackFunc *b, NSInteger x) {
  __block NSInteger result = 0;
  dispatch_semaphore_t sem = dispatch_semaphore_create(0);
  [b compute:x completionHandler:^(NSInteger r) {
    result = r;
    dispatch_semaphore_signal(sem);
  }];
  dispatch_semaphore_wait(sem, DISPATCH_TIME_FOREVER);
  return result;
}

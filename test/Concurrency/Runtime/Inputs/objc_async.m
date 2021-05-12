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

@implementation MutableButt: Butt
@end

@implementation MutableButt_2Fast2Furious: MutableButt
@end


@implementation Farm

-(void)getDogWithCompletion:(void (^ _Nonnull)(NSInteger))completionHandler {
  printf("getting dog\n");
  completionHandler(123);
}

-(void)obtainCat:(void (^ _Nonnull)(NSInteger, NSError* _Nullable))completionHandler {
  printf("obtaining cat has failed!\n");
  completionHandler(nil, [NSError errorWithDomain:@"obtainCat" code:456 userInfo:nil]);
}

@end

void scheduleButt(Butt *b, NSString *s) {
  [b butt: 1738 completionHandler: ^(NSInteger i) {
    printf("butt %p named %s occurred at %zd\n", b, [s UTF8String], (ssize_t)i);
    fflush(stdout);
  }];
}

#import <Foundation/Foundation.h>

static inline id _Nullable CatchingException(void (^_Nonnull block)(void)) {
  @try {
    block();
  } @catch (id e) {
    return e;
  }
  return nil;
}

static inline void ThrowObjCException(void) {
  @throw [NSException exceptionWithName:@"test"
                                 reason:@"I feel like it"
                               userInfo:nil];
}

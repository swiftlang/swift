#import <Foundation/Foundation.h>

static inline void takesBlockWithConsumedArg(void (^ block)(NS_RELEASES_ARGUMENT NSObject *x), NSObject *x) {
  block(x);
}

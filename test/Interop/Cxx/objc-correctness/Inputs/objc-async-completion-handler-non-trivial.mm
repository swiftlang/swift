#import "objc-async-completion-handler-non-trivial.h"

int Tracked::liveCount = 0;

@implementation TrackedProducer
- (void)produceWithCompletionHandler:(void (^)(Tracked))completion {
  Tracked t;
  completion(t);
}
@end

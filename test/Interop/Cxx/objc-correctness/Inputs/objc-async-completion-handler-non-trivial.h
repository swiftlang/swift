#pragma once

#import <Foundation/Foundation.h>

struct Tracked {
  static int liveCount;

  Tracked() { ++liveCount; }
  Tracked(const Tracked &) { ++liveCount; }
  Tracked(Tracked &&) = delete;
  Tracked &operator=(const Tracked &) = delete;
  Tracked &operator=(Tracked &&) = delete;
  ~Tracked() { --liveCount; }
};

inline int getTrackedLiveCount() { return Tracked::liveCount; }

NS_ASSUME_NONNULL_BEGIN
@interface TrackedProducer : NSObject
// Imported into Swift both as `produce(completionHandler:)` and as the async
// `produce() async -> Tracked`.
- (void)produceWithCompletionHandler:(void (^)(Tracked))completion;
@end
NS_ASSUME_NONNULL_END

@import Foundation;

typedef NS_ENUM(NSInteger, BackgroundActivityResult) {
    BackgroundActivityResultFinished = 1,
    BackgroundActivityResultDeferred = 2,
};

typedef void (^BackgroundActivityCompletionHandler)(BackgroundActivityResult result);

@interface BackgroundActivityScheduler : NSObject
- (void)scheduleWithBlock:(void (^)(BackgroundActivityCompletionHandler completionHandler))block;
@end

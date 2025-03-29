
@import Foundation;

@interface MyNotificationCenter
- (id)init;
- (void)post;
@end

@protocol MySession <NSObject>
- (void)endSession;
@end

typedef NSString *MyStringEnum NS_EXTENSIBLE_STRING_ENUM;

@interface MyAssetTrack : NSObject
@end

@interface MyAsset : NSObject

- (void)loadTracksWithStringEnum:(MyStringEnum)stringEnum completionHandler:(void (^)(NSArray<MyAssetTrack *> * _Nullable, NSError * _Nullable)) completionHandler;

@end


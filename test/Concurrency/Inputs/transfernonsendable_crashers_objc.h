
@import Foundation;

@interface MyNotificationCenter
- (id)init;
- (void)post;
@end

@protocol MySession <NSObject>
- (void)endSession;
@end

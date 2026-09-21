@import Foundation;

@protocol P
@optional
- (void) f;
@end

@interface C: NSObject <P>
- (void) f;
@end
@import Foundation;

@interface ErrorProne : NSObject
+ (BOOL) fail: (NSError**) error;
@end

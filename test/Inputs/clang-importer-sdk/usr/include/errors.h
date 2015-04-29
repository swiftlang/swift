@import Foundation;

@interface ErrorProne : NSObject
+ (BOOL) fail: (NSError**) error;
+ (BOOL) goAndReturnError: (NSError**) error;
+ (BOOL) tryAndReturnError: (NSError**) error;
@end

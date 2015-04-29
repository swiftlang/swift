/* -*- ObjC -*- */

@import Foundation;

@interface ErrorProne : NSObject
+ (BOOL) fail: (NSError**) error;
+ (BOOL) goAndReturnError: (NSError**) error;
+ (BOOL) tryAndReturnError: (NSError**) error;

+ (BOOL) consume: (id) object error: (NSError**) error;
@end

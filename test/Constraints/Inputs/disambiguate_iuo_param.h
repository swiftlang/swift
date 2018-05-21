@import Foundation;

@interface NSObject (NSMyTest)
- (BOOL)isEqualTo:(nullable id)object;
@end

@interface Obj : NSObject
- (BOOL)isEqualToObject:(id)value;
@end

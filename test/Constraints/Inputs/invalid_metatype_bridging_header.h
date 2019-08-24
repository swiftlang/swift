@import Foundation;

// rdar://problem/33830526: Constraint system should not add static methods
// to the overload search space if it would require bridging unrelated metatypes.
@interface NSString (Extension)
+ (void) meth;
@end

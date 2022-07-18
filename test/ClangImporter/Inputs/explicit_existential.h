@import Foundation;

@protocol P
@end

@protocol Q
@end

typedef id<P, Q> PAndQ;

@interface PAndQProcessor : NSObject
- (void) takesPAndQExistential: (PAndQ)arg;
@end
@import Foundation;

@interface Base
- (void)overriddenInOverlayForA __attribute__((deprecated("Categories_A.h")));
- (void)overriddenInOverlayForB __attribute__((deprecated("Categories_A.h")));
- (void)overriddenInOverlayForC __attribute__((deprecated("Categories_A.h")));
- (void)overriddenInSubclassInOverlayForC __attribute__((deprecated("Categories_A.h")));
@end

@interface X : Base
@end

@interface X (A)
- (void)fromA;
@end

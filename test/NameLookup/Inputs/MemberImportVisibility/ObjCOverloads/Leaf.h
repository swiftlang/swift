@import Branch;

@interface LeafObject : BranchObject
- (void)overridden1 __attribute__((deprecated("Leaf.h")));
@end

@interface BranchObject (Leaf)
- (void)overridden2 __attribute__((deprecated("Leaf.h")));
- (void)overridden4 __attribute__((deprecated("Leaf.h")));
@end

@interface LeafObject (Leaf)
- (void)overridden3 __attribute__((deprecated("Leaf.h")));
@end


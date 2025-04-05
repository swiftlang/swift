@import Root;

@interface BranchObject : RootObject
- (void)overridden1 __attribute__((deprecated("Branch.h")));
@end

@interface BranchObject (Branch)
- (void)overridden3 __attribute__((deprecated("Branch.h")));
@end

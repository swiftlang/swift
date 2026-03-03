@import Branch;

@interface FruitObject : BranchObject
- (void)overridden1 __attribute__((deprecated("Fruit.h")));
@end

@interface FruitObject (Fruit)
- (void)overridden4 __attribute__((deprecated("Fruit.h")));
@end

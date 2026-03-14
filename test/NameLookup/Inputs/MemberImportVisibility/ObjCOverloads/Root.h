@interface RootObject
- (instancetype)init;
- (void)overridden1 __attribute__((deprecated("Root.h")));
- (void)overridden2 __attribute__((deprecated("Root.h")));
@end

@interface RootObject (Root)
- (void)overridden3 __attribute__((deprecated("Root.h")));
- (void)overridden4 __attribute__((deprecated("Root.h")));
@end

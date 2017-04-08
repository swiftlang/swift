@interface Object
- (nonnull instancetype)init;
@end

@interface Base : Object
#ifndef BAD
- (void)method;
#endif
@end

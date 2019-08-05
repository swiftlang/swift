@interface Root
- (instancetype)init;
@end

#if !BAD
@interface DisappearingSuperclass : Root
@end
#else
#endif

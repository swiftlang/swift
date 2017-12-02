
@protocol P
@optional
- (void)method;
@end

@interface I
- (instancetype _Nonnull)init;
@end

I * _Nonnull f(I * _Nonnull);


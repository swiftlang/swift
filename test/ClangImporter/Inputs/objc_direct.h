@interface Bar
@property (readonly, direct, nonatomic) int directProperty;
- (void)directMethod __attribute__((objc_direct));
+ (void)directClassMethod __attribute__((objc_direct));
@end

__attribute__((objc_direct_members))
@interface Bar ()
@property (readonly, nonatomic) int directProperty2;
- (void)directMethod2;
+ (void)directClassMethod2;
@end

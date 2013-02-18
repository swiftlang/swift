@import ObjectiveC;

@interface B(Separate)
- (id)method:(int)i separateExtMethod:(double)d;
- (void)anotherCategoryMethod;
@end

@class RefedSub;

@interface SuperRefsSub
- takesSub:(RefedSub*)sub;
- overridden;
@end

@interface RefedSub : SuperRefsSub
- overridden;
@end

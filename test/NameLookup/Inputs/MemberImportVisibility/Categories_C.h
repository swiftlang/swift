@import Categories_A;

@interface X (C)
- (void)fromC;
@end

@interface SubclassFromC : X
- (instancetype)init;
@end

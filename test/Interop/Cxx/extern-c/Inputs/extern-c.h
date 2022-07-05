@interface A
@end

extern "C"
@interface A (CAT1)
- (int)foo;
@end

@interface A (CAT2)
- (int)bar;
@end

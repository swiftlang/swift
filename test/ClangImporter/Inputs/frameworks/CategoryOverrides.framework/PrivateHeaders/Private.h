#import <CategoryOverrides/CategoryOverrides.h>

@interface MyBaseClass ()
@property (nonatomic, strong, nullable) Base *derivedMember;
@end

@interface MyColor ()
+ (MyColor * _Null_unspecified) systemRedColor;
@end

@protocol MyPrivateProtocol
- (SomeStruct) myStructure;
@end

@interface MyBaseClass () <MyPrivateProtocol>
@end

@interface Refinery ()
@property (nonatomic, readwrite) RefinedSugar sugar;
@end

@interface ExtraRefinery ()
- (void)setSugar:(RefinedSugar)sugar;
@end

@interface MyBaseClass () <NonNullProtocol>
@end

@interface MyDerivedClass () <ReadwriteProtocol>
@end

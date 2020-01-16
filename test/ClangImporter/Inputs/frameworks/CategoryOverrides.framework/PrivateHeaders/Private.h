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

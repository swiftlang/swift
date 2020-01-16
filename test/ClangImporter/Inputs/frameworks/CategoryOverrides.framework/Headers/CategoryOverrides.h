__attribute__((objc_root_class))
@interface Base
- (nonnull instancetype)init;
@end

@interface MyColor : Base
@property (class, nonatomic, readonly) MyColor *systemRedColor;
@end

@interface MyBaseClass : Base
// @property (nonatomic, strong, nullable) Base *derivedMember;
@end

@interface MyDerivedClass : MyBaseClass
@property (nonatomic, strong, nullable) Base *derivedMember;
@end

__attribute__((objc_root_class))
@interface Base
- (nonnull instancetype)init;
@end

typedef struct SomeStruct_s {
  int inner;
} SomeStruct;

@interface MyColor : Base
@property (class, nonatomic, readonly) MyColor *systemRedColor;
@end

@interface MyBaseClass : Base
// @property (nonatomic, strong, nullable) Base *derivedMember;
@property (nonatomic, assign, readonly) SomeStruct myStructure;
@end

@interface MyDerivedClass : MyBaseClass
@property (nonatomic, strong, nullable) Base *derivedMember;
@end

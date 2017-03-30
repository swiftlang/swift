__attribute__((objc_root_class))
@interface Base
- (nonnull instancetype)init;
@end

@interface GenericClass<T>: Base
@end

@interface PropertiesInit : Base
@property (readonly, nonnull) Base *nullabilityChange;
@property (readonly, nonnull) GenericClass<Base *> *missingGenerics;
@property (readonly, nonnull) Base *typeChange;
@end

@interface PropertiesNoInit : Base
@property (readonly, nonnull) Base *nullabilityChange;
@property (readonly, nonnull) GenericClass<Base *> *missingGenerics;
@property (readonly, nonnull) Base *typeChange;
@end

@interface PropertiesInitCategory : Base
@end

@interface PropertiesInitCategory (Category)
@property (readonly, nonnull) Base *nullabilityChange;
@property (readonly, nonnull) GenericClass<Base *> *missingGenerics;
@property (readonly, nonnull) Base *typeChange;
@end

@interface PropertiesNoInitCategory : Base
@end

@interface PropertiesNoInitCategory (Category)
@property (readonly, nonnull) Base *nullabilityChange;
@property (readonly, nonnull) GenericClass<Base *> *missingGenerics;
@property (readonly, nonnull) Base *typeChange;
@end

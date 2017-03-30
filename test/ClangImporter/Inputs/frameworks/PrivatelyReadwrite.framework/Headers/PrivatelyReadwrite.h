__attribute__((objc_root_class))
@interface Base
- (nonnull instancetype)init;
@end

@interface GenericClass<T>: Base
@end

@interface PropertiesA : Base
@property (readonly, nonnull) Base *nullabilityChange;
@property (readonly, nonnull) GenericClass<Base *> *missingGenerics;
@property (readonly, nonnull) Base *typeChange;
@end

@interface PropertiesB : Base
@property (readonly, nonnull) Base *nullabilityChange;
@property (readonly, nonnull) GenericClass<Base *> *missingGenerics;
@property (readonly, nonnull) Base *typeChange;
@end

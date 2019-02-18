@interface Base
@end
@interface ImportedClass : Base
- (nonnull instancetype)init __attribute__((objc_designated_initializer));
- (nonnull instancetype)initConveniently;
+ (nonnull instancetype)importedClassWithConvenientFactory:(_Bool)unused;
+ (nonnull ImportedClass *)importedClassWithNormalFactory:(_Bool)unused;
@end

@interface Sub : ImportedClass
@end

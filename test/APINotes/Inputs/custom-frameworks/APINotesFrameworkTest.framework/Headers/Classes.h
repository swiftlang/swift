#ifdef __OBJC__
#pragma clang assume_nonnull begin

@interface NewlyGenericSub<Element> : Base
+ (Element)defaultElement;
@end

@interface RenamedGeneric<Element: Base *> : Base
@end

@interface ClassWithManyRenames : Base
+ (instancetype)classWithManyRenamesForInt:(int)value;
- (instancetype)initWithBoolean:(_Bool)value __attribute__((swift_name("init(finalBoolean:)")));

- (void)doImportantThings __attribute__((swift_name("finalDoImportantThings()")));
@property (class, nullable) id importantClassProperty __attribute__((swift_name("finalClassProperty")));
@property (nullable) id importantInstanceProperty __attribute__((swift_name("finalInstanceProperty")));
@end

#pragma clang assume_nonnull end
#endif // __OBJC__

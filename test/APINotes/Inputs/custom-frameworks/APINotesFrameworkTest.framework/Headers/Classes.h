#ifdef __OBJC__
#pragma clang assume_nonnull begin

@interface NewlyGenericSub<Element> : Base
+ (Element)defaultElement;
@end

@interface RenamedGeneric<Element: Base *> : Base
@end

#pragma clang assume_nonnull end
#endif // __OBJC__

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retainCFRefCounted")))
__attribute__((swift_attr("release:releaseCFRefCounted"))) CFRefCounted {
  int value = 0;

  static CFRefCounted *_Nonnull staticCreateNotRetained()
      __attribute__((cf_returns_not_retained));
  static CFRefCounted *_Nonnull staticGetRetained()
      __attribute__((cf_returns_retained));

  CFRefCounted *_Nonnull methodNotRetained() const
      __attribute__((cf_returns_not_retained));
  CFRefCounted *_Nonnull methodRetained() const
      __attribute__((cf_returns_retained));
};

void retainCFRefCounted(CFRefCounted *_Nonnull);
void releaseCFRefCounted(CFRefCounted *_Nonnull);

CFRefCounted *_Nonnull getCFRefCountedNotRetained()
    __attribute__((cf_returns_not_retained));
// The "Create" rule would otherwise make this +1.
CFRefCounted *_Nonnull createCFRefCountedNotRetained()
    __attribute__((cf_returns_not_retained));
CFRefCounted *_Nonnull getCFRefCountedRetained()
    __attribute__((cf_returns_retained));

#ifdef __OBJC__
@interface CFRefCountedProvider
+ (CFRefCounted *_Nonnull)classGetNotRetained
    __attribute__((cf_returns_not_retained));
+ (CFRefCounted *_Nonnull)classGetRetained
    __attribute__((cf_returns_retained));
@end
#endif

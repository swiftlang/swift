struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retainFRT")))
__attribute__((swift_attr("release:releaseFRT"))) FRT {
  static FRT *_Nonnull create();
};
void retainFRT(FRT *);
void releaseFRT(FRT *);

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retainOptionalFRT")))
__attribute__((swift_attr("release:releaseOptionalFRT"))) OptionalFRT {
  static OptionalFRT *_Nullable create();
};
void retainOptionalFRT(OptionalFRT *);
void releaseOptionalFRT(OptionalFRT *);

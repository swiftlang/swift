extern void fromUnderlyingClang(void);

struct ClangType {
  int instanceMember;
};

extern void ClangTypeImportAsMemberStaticMethod(void)
    __attribute__((swift_name("ClangType.importAsMemberStaticMethod()")));

extern void ClangTypeImportAsMemberInstanceMethod(struct ClangType *)
    __attribute__((swift_name("ClangType.importAsMemberInstanceMethod(self:)")));

typedef int ClangTypeInner __attribute__((swift_name("ClangType.Inner")));

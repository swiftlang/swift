#if !BAD
typedef int MysteryTypedef;
#else
typedef _Bool MysteryTypedef;
#endif

struct ImportedType {
  int value;
};

typedef MysteryTypedef ImportedTypeAssoc __attribute__((swift_name("ImportedType.Assoc")));

#if !BAD
typedef int WrappedInt __attribute__((swift_wrapper(struct)));
typedef int UnwrappedInt;
#else
typedef int WrappedInt;
typedef int UnwrappedInt __attribute__((swift_wrapper(struct)));
#endif

#if !BAD
typedef int MysteryTypedef;
#else
typedef _Bool MysteryTypedef;
#endif

struct ImportedType {
  int value;
};

typedef MysteryTypedef ImportedTypeAssoc __attribute__((swift_name("ImportedType.Assoc")));

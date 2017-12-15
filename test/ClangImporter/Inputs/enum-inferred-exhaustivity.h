#if defined(CF_ENUM)
# error "This test requires controlling the definition of CF_ENUM"
#endif

// Make this C-compatible by leaving out the type.
#define CF_ENUM(_name) enum _name _name; enum _name

typedef CF_ENUM(EnumWithDefaultExhaustivity) {
  EnumWithDefaultExhaustivityLoneCase
};

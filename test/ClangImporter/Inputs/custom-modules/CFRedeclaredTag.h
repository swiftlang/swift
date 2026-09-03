// ClangImporter should pick up the `objc_bridge` attribute even though it is
// only present on the second declaration, not the first...
typedef struct __CFRedeclaredTag CFRedeclaredTag;
typedef struct __attribute__((objc_bridge(id))) __CFRedeclaredTag *CFRedeclaredTagRef;

// ...and should therefore import this with a non-`Unmanaged` return type.
extern CFRedeclaredTagRef CFRedeclaredTagCreate(void);

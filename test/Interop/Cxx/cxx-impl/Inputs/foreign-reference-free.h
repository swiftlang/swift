#ifndef TEST_INTEROP_CXX_CXX_IMPL_FOREIGN_REFERENCE_FREE_H
#define TEST_INTEROP_CXX_CXX_IMPL_FOREIGN_REFERENCE_FREE_H

struct Object;
void retainObject(Object *_Nonnull);
void releaseObject(Object *_Nonnull);

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retainObject")))
__attribute__((swift_attr("release:releaseObject"))) Object {
  int refs = 1;
  bool freed = false;

  // The key function, in C++.
  virtual void anchor() const;
  virtual void free();
};

struct Derived : Object {
  // The key function, in C++.
  void anchor() const override;
  // In Swift.
  void free() override;
};

// In Swift.
void inspect(Object *_Nonnull object);

void note(const char *_Nonnull message);
void noteRefs(int refs);

#endif // !TEST_INTEROP_CXX_CXX_IMPL_FOREIGN_REFERENCE_FREE_H

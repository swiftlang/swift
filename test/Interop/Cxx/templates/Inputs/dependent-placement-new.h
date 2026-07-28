#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_DEPENDENT_PLACEMENT_NEW_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_DEPENDENT_PLACEMENT_NEW_H

void *operator new(__SIZE_TYPE__, void *p) noexcept;

struct Widget {
  int value;
};

// A generic lambda's call operator is a member function template of the
// closure type. In its *uninstantiated* pattern the placement-new allocates a
// dependent type (decltype(value)), so clang leaves
// CXXNewExpr::getOperatorNew() == nullptr. IRGen's ClangDeclFinder used to
// descend into this pattern while looking for referenced declarations and
// crashed dereferencing the null operator-new. It must only ever look at
// concrete instantiations, never the dependent template pattern.
inline Widget makeWidget() {
  Widget result;
  auto make = [](void *storage, auto value) {
    ::new (storage) decltype(value)(value);
  };
  make(&result, Widget{42});
  return result;
}

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_DEPENDENT_PLACEMENT_NEW_H

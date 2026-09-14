#ifndef TEST_INTEROP_CXX_CLASS_METHOD_UNSAFE_PROJECTIONS_H
#define TEST_INTEROP_CXX_CLASS_METHOD_UNSAFE_PROJECTIONS_H

#include <string>

struct NestedSelfContained;
struct Empty;
struct SelfContained;
struct ExplicitSelfContained;
struct NestedExplicitSelfContained;

struct View {
  void *ptr;
  
  void *data() const;
  void *empty() const;
  std::string name() const;
  NestedSelfContained nested() const;
  ExplicitSelfContained explicitSelfContained() const;
  NestedExplicitSelfContained explicitNested() const;
};

// expected-strict-note@+1 {{this type has unknown escapability: Swift cannot infer it from the type's members; annotate the type with SWIFT_ESCAPABLE or SWIFT_NONESCAPABLE}}
struct SelfContained {
  void *ptr;
  SelfContained(const SelfContained&);
  
  std::string name() const;
  SelfContained selfContained() const;
  NestedSelfContained nested() const;
  Empty empty() const;
  int value() const;
  // expected-note@+1 {{this returns a view into a type that owns its storage}}
  View view() const;
  // expected-default-note@+1 {{this returns a pointer or reference into a type that owns its storage}}
  int *pointer() const;
  ExplicitSelfContained explicitSelfContained() const;
  NestedExplicitSelfContained explicitNested() const;
};

struct NestedSelfContained {
  SelfContained member;
  
  std::string name() const;
  SelfContained selfContained() const;
  NestedSelfContained nested() const;
  Empty empty() const;
  int value() const;
  View view() const;
  int *pointer() const;
  ExplicitSelfContained explicitSelfContained() const;
  NestedExplicitSelfContained explicitNested() const;
};

struct InheritSelfContained: SelfContained {
  std::string name() const;
  SelfContained selfContained() const;
  NestedSelfContained nested() const;
  Empty empty() const;
  int value() const;
  View view() const;
  int *pointer() const;
};

struct __attribute__((swift_attr("import_owned"))) ExplicitSelfContained {
  void *ptr;
  
  void *pointer() const;
  View view() const;
  NestedSelfContained nested() const;
};

struct NestedExplicitSelfContained {
  ExplicitSelfContained m;

  SelfContained selfContained() const;
  NestedSelfContained nested() const;
  int value() const;
  View view() const;
  int *pointer() const;
};

struct Empty {
  Empty empty() const;
  void *pointer() const;
  SelfContained selfContained() const;
};

struct IntPair {
  int a; int b;

  int first() const;
  void *pointer() const;
  SelfContained selfContained() const;
};

#endif // TEST_INTEROP_CXX_CLASS_METHOD_UNSAFE_PROJECTIONS_H

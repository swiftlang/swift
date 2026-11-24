#ifndef TEST_INTEROP_CXX_CLASS_METHOD_ALWAYS_UNSAFE_PROJECTIONS_H
#define TEST_INTEROP_CXX_CLASS_METHOD_ALWAYS_UNSAFE_PROJECTIONS_H

struct InheritedView {
  void *ptr;
};

struct InheritedBase {
  void *ptr;
  InheritedBase(const InheritedBase &);

  // expected-note@+1 {{this returns a view into a type that owns its storage}}
  InheritedView view() const;
  int *pointer() const;
  int value() const;
};

// Inherits 'view()' and 'pointer()' without redeclaring them, so lookup has to
// clone both the original-named member and its '__<name>Unsafe' stub, and the
// clone has to keep '@unsafe(always)'.
struct InheritedDerived : InheritedBase {};

// 'value', 'insert' and 'append' used to be carved out of the rename for the
// C++ standard library, where the overlay provides same-named safe wrappers.
// Every type now gets the ordinary treatment.
struct NotStd {
  int x;
  NotStd(const NotStd &);

  int *value();
  int *insert(int);
  int *append(int);
};

struct TemplateAndSafeOwner {
  int *storage;
  TemplateAndSafeOwner(const TemplateAndSafeOwner &);

  // Returns whatever the caller passes in, not a projection of 'this'.
  template <typename T> T identity(T t) const { return t; }

  // 'safe' exempts a method from the heuristic.
  __attribute__((swift_attr("safe"))) int *vouchedProjection() const;

  // ...except for begin and end, whose '__<name>Unsafe' stubs witness the
  // conformance to CxxConvertibleToCollection.
  // expected-note@+1 {{'begin' and 'end' are assumed to return iterators, which do not keep the underlying storage alive}}
  __attribute__((swift_attr("safe"))) const int *begin() const;
  __attribute__((swift_attr("safe"))) const int *end() const;
};

#endif // TEST_INTEROP_CXX_CLASS_METHOD_ALWAYS_UNSAFE_PROJECTIONS_H

#ifndef TEST_INTEROP_CXX_CLASS_METHOD_ALWAYS_UNSAFE_PROJECTIONS_H
#define TEST_INTEROP_CXX_CLASS_METHOD_ALWAYS_UNSAFE_PROJECTIONS_H

struct InheritedView {
  void *ptr;
};

struct InheritedBase {
  void *ptr;
  InheritedBase(const InheritedBase &);

  InheritedView view() const;
  int *pointer() const;
  int value() const;
};

// Inherits 'view()' and 'pointer()' without redeclaring them, so lookup has to
// clone both the original-named member and its '__<name>Unsafe' stub, and the
// clone has to keep '@unsafe(always)'.
struct InheritedDerived : InheritedBase {};

// 'value', 'insert' and 'append' are only carved out for the C++ standard
// library, where the overlay provides same-named safe wrappers. A user type
// gets the ordinary treatment.
struct NotStd {
  int x;
  NotStd(const NotStd &);

  int *value();
  int *insert(int);
  int *append(int);
};

#endif // TEST_INTEROP_CXX_CLASS_METHOD_ALWAYS_UNSAFE_PROJECTIONS_H

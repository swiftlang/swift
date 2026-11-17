#ifndef TEST_INTEROP_CXX_CXX_IMPL_LIFETIMES_H
#define TEST_INTEROP_CXX_CXX_IMPL_LIFETIMES_H

#include <swift/bridging>

// A class with a user-provided copy constructor and destructor.
struct Tracked {
  int value;
  Tracked(int v);
  Tracked(const Tracked &other);
  ~Tracked();
};

// A non-escapable view of a `Tracked`.
struct SWIFT_NONESCAPABLE TrackedView {
  const int *value;
};

// A lifetimebound `const T &` parameter imports as `borrowing`.

TrackedView viewTracked(const Tracked &t [[clang::lifetimebound]]);

#endif

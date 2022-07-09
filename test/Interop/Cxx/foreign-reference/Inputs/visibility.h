#ifndef TEST_INTEROP_CXX_FOREIGN_REFERENCE_INPUTS_VISIBILITY_H
#define TEST_INTEROP_CXX_FOREIGN_REFERENCE_INPUTS_VISIBILITY_H

#if __has_feature(nullability)
// Provide macros to temporarily suppress warning about the use of
// _Nullable and _Nonnull.
# define SWIFT_BEGIN_NULLABILITY_ANNOTATIONS                                   \
  _Pragma("clang diagnostic push")                                             \
  _Pragma("clang diagnostic ignored \"-Wnullability-extension\"")              \
  _Pragma("clang assume_nonnull begin")
# define SWIFT_END_NULLABILITY_ANNOTATIONS                                     \
  _Pragma("clang diagnostic pop")                                              \
  _Pragma("clang assume_nonnull end")

#else
// #define _Nullable and _Nonnull to nothing if we're not being built
// with a compiler that supports them.
# define _Nullable
# define _Nonnull
# define _Null_unspecified
# define SWIFT_BEGIN_NULLABILITY_ANNOTATIONS
# define SWIFT_END_NULLABILITY_ANNOTATIONS
#endif

#endif // TEST_INTEROP_CXX_FOREIGN_REFERENCE_INPUTS_VISIBILITY_H

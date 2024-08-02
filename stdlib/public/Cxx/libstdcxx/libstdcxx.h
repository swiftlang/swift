#include "version"

// C++17 and newer:

// <execution> includes tbb headers, which might not be installed.
// Only include <execution> if tbb is installed.
#if __has_include("execution") && __has_include(<tbb/blocked_range.h>) && (_GLIBCXX_RELEASE < 11)
#include "execution"
#endif

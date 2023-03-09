// libstdc++ 4.8.5 bundled with CentOS 7 does not include corecvt.
#if __has_include("codecvt")
#include "codecvt"
#endif

// C++17 and newer:

#if __has_include("any")
#include "any"
#endif
#if __has_include("charconv")
#include "charconv"
#endif
// <execution> includes tbb headers, which might not be installed.
// Only include <execution> if tbb is installed.
#if __has_include("execution") && __has_include(<tbb/blocked_range.h>)
#include "execution"
#endif
#if __has_include("filesystem")
#include "filesystem"
#endif
#if __has_include("memory_resource")
#include "memory_resource"
#endif
#if __has_include("optional")
#include "optional"
#endif
#if __has_include("string_view")
#include "string_view"
#endif
#if __has_include("variant")
#include "variant"
#endif

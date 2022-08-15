#include "algorithm"
#include "bitset"
#include "complex"
#include "deque"
#include "exception"
#include "fstream"
#include "functional"
#include "iomanip"
#include "ios"
#include "iosfwd"
#include "iostream"
#include "istream"
#include "iterator"
#include "limits"
#include "list"
#include "locale"
#include "map"
#include "memory"
#include "new"
#include "numeric"
#include "ostream"
#include "queue"
#include "set"
#include "sstream"
#include "stack"
#include "stdexcept"
#include "streambuf"
#include "string"
#include "utility"
#include "typeinfo"
#include "valarray"
#include "vector"
#include "array"
#include "atomic"
#include "chrono"
#include "condition_variable"
#include "forward_list"
#include "future"
#include "initializer_list"
#include "mutex"
#include "random"
#include "ratio"
#include "regex"
#include "scoped_allocator"
#include "system_error"
#include "thread"
#include "tuple"
#include "typeindex"
#include "type_traits"
#include "unordered_map"
#include "unordered_set"

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
#if __has_include("execution")
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

#include <iostream>

int main() {
#if !defined(_LIBCPP_VERSION)
  std::cout << "no libc++ found\n";
  return 1;
#endif

  // the libc++ module was split into multiple top-level modules in Clang 17,
  // and then re-merged into one module with submodules in Clang 20
#if _LIBCPP_VERSION >= 170004 && _LIBCPP_VERSION < 200000
  return 0;
#else
  return 1;
#endif
}

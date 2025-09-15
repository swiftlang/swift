#include <iostream>

int main() {
#if !defined(_LIBCPP_VERSION)
  std::cout << "no libc++ found\n";
  return 1;
#endif

#if _LIBCPP_VERSION >= 170004
  return 0;
#else
  return 1;
#endif
}

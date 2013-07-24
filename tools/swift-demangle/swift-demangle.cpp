#include "swift/SIL/Demangle.h"

#include <string>
#include <iostream>

int main(int argc, char **argv) {
    for (int k = 1; k < argc; k++) {
    if (strcmp(argv[k], "--type") == 0) {
      if (++k >= argc)
        break;
      std::cout << argv[k] << " ---> " << swift::Demangle::demangleType(argv[k])
                << std::endl;
    } else {
      std::cout << argv[k] << " ---> "
                << swift::Demangle::demangleSymbol(argv[k]) << std::endl;
    }
  }
  return 0;
}

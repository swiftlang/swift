#ifndef TEST_INTEROP_CXX_IMPLEMENTATION_ONLY_IMPORTS_INPUTS_HELPER_H
#define TEST_INTEROP_CXX_IMPLEMENTATION_ONLY_IMPORTS_INPUTS_HELPER_H

inline int getFortyTwo() { return 42; };

class MagicWrapper {
  int _number;

public:
  MagicWrapper(){};
  MagicWrapper(int number) : _number(number){};
};

#endif // TEST_INTEROP_CXX_IMPLEMENTATION_ONLY_IMPORTS_INPUTS_HELPER_H

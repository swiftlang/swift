#ifndef TEST_INTEROP_CXX_STDLIB_INPUTS_STD_FUNCTION_H
#define TEST_INTEROP_CXX_STDLIB_INPUTS_STD_FUNCTION_H

#include <functional>

using FunctionIntToInt = std::function<int(int)>;

FunctionIntToInt getIdentityFunction() {
  return [](int x) { return x; };
}

bool isEmptyFunction(FunctionIntToInt f) { return !(bool)f; }

int invokeFunction(FunctionIntToInt f, int x) { return f(x); }

#endif // TEST_INTEROP_CXX_STDLIB_INPUTS_STD_FUNCTION_H

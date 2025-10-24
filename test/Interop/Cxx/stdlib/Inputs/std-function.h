#ifndef TEST_INTEROP_CXX_STDLIB_INPUTS_STD_FUNCTION_H
#define TEST_INTEROP_CXX_STDLIB_INPUTS_STD_FUNCTION_H

#include <functional>
#include <string>

using FunctionVoidToVoid = std::function<void()>;
using FunctionVoidToInt = std::function<int()>;
using FunctionIntToVoid = std::function<void(int)>;
using FunctionIntToInt = std::function<int(int)>;
using FunctionConstIntToInt = std::function<int(const int)>;
using FunctionConstRefIntToInt = std::function<int(const int&)>;
using FunctionIntIntToInt = std::function<int(int, int)>;
using FunctionStringToString = std::function<std::string(std::string)>;
using FunctionStringToStringConstRef = std::function<std::string(const std::string&)>;
using FunctionTooManyParams = std::function<int(int, int, int, int, int, int, int)>;

inline FunctionIntToInt getIdentityFunction() {
  return [](int x) { return x; };
}

inline bool isEmptyFunction(FunctionIntToInt f) { return !(bool)f; }

inline int invokeFunction(FunctionIntToInt f, int x) { return f(x); }

int invokeFunctionIntToIntTwice(FunctionIntToInt f, int i) {
  return f(f(i));
}
std::string invokeFunctionTwice(FunctionStringToString f, std::string s) {
  return f(f(s));
}

std::string invokeFunctionTwiceConstRef(FunctionStringToStringConstRef f, std::string s) {
  return f(f(s));
}

#endif // TEST_INTEROP_CXX_STDLIB_INPUTS_STD_FUNCTION_H

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
using FunctionConstRefStringToString = std::function<std::string(const std::string&)>;
using FunctionConstRefStringToConstRefString = std::function<const std::string&(const std::string&)>;

struct HasDeletedCopyCtor {
  int value;
  HasDeletedCopyCtor(int value) : value(value) {}
  HasDeletedCopyCtor(const HasDeletedCopyCtor &other) = delete;
  HasDeletedCopyCtor(HasDeletedCopyCtor &&other) = default;
  HasDeletedCopyCtor& operator=(const HasDeletedCopyCtor &other) = delete;
  HasDeletedCopyCtor& operator=(HasDeletedCopyCtor &&other) = default;
  ~HasDeletedCopyCtor() = default;
};
using FunctionIntToHasDeletedCopyCtor = std::function<HasDeletedCopyCtor(int)>;
using FunctionConstRefHasDeletedCopyCtorToVoid = std::function<void(const HasDeletedCopyCtor&)>;
using FunctionConstRefHasDeletedCopyCtorToInt = std::function<int(const HasDeletedCopyCtor&)>;
using FunctionHasDeletedCopyCtor = std::function<HasDeletedCopyCtor(const HasDeletedCopyCtor&)>;

int invokeFunctionConstRefHasDeletedCopyCtorToInt(FunctionConstRefHasDeletedCopyCtorToInt f) {
  HasDeletedCopyCtor arg(123);
  return f(arg);
}

struct NonTrivialHasDeletedCopyCtor {
  int value;
  bool destroyed = false;
  NonTrivialHasDeletedCopyCtor(int value) : value(value) {}
  NonTrivialHasDeletedCopyCtor(const NonTrivialHasDeletedCopyCtor &other) = delete;
  NonTrivialHasDeletedCopyCtor(NonTrivialHasDeletedCopyCtor &&other) = default;
  NonTrivialHasDeletedCopyCtor& operator=(const NonTrivialHasDeletedCopyCtor &other) = delete;
  NonTrivialHasDeletedCopyCtor& operator=(NonTrivialHasDeletedCopyCtor &&other) = default;
  ~NonTrivialHasDeletedCopyCtor() { destroyed = true; } // makes the type non-trivial
};
using FunctionIntToNonTrivialHasDeletedCopyCtor = std::function<NonTrivialHasDeletedCopyCtor(int)>;
using FunctionConstRefNonTrivialHasDeletedCopyCtorToVoid = std::function<void(const NonTrivialHasDeletedCopyCtor&)>;
using FunctionNonTrivialHasDeletedCopyCtor = std::function<NonTrivialHasDeletedCopyCtor(const NonTrivialHasDeletedCopyCtor&)>;

inline FunctionIntToInt getIdentityFunction() {
  return [](int x) { return x; };
}

inline bool isEmptyFunction(FunctionIntToInt f) { return !(bool)f; }

inline int invokeFunction(FunctionIntToInt f, int x) { return f(x); }

int invokeFunctionIntToIntTwice(FunctionIntToInt f, int i) {
  return f(f(i));
}
int invokeFunctionIntToIntByConstRefTwice(const FunctionIntToInt& f, int i) {
  return f(f(i));
}
int invokeFunctionIntToIntByRValueRefTwice(const FunctionIntToInt& f, int i) {
  return f(f(i));
}

std::string invokeFunctionTwice(FunctionStringToString f, std::string s) {
  return f(f(s));
}
std::string invokeFunctionByConstRefTwice(const FunctionStringToString& f, std::string s) {
  return f(f(s));
}

std::string invokeFunctionTwiceConstRef(FunctionConstRefStringToString f, std::string s) {
  return f(f(s));
}

std::string invokeFunctionTwiceConstRefX2(FunctionConstRefStringToConstRefString f, std::string s) {
  return f(f(s));
}

template<typename Func>
int invokeTemplatedCallableIntToInt(Func f) { return f(123); };
template<typename Func>
int invokeTemplatedCallableByConstRefIntToInt(const Func& f) { return f(321); };

#endif // TEST_INTEROP_CXX_STDLIB_INPUTS_STD_FUNCTION_H

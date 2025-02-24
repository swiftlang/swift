#ifndef TEST_INTEROP_CXX_STDLIB_INPUTS_STD_SPAN_H
#define TEST_INTEROP_CXX_STDLIB_INPUTS_STD_SPAN_H

#include <cstddef>
#include <span>
#include <string>
#include <vector>

using ConstSpanOfInt = std::span<const int>;
using SpanOfInt = std::span<int>;
using ConstSpanOfString = std::span<const std::string>;
using SpanOfString = std::span<std::string>;
using VecOfInt = std::vector<int>;

static int iarray[]{1, 2, 3};
static std::string sarray[]{"", "ab", "abc"};
static ConstSpanOfInt icspan = {iarray};
static SpanOfInt ispan = {iarray};
static ConstSpanOfString scspan = {sarray};
static SpanOfString sspan = {sarray};

struct SpanBox {
  ConstSpanOfInt icspan;
  SpanOfInt ispan;
  ConstSpanOfString scspan;
  SpanOfString sspan;
};

class CppApi {
public:
  ConstSpanOfInt getConstSpan();
  SpanOfInt getSpan();
};

ConstSpanOfInt CppApi::getConstSpan() {
  ConstSpanOfInt sp{new int[2], 2};
  return sp;
}

SpanOfInt CppApi::getSpan() {
  SpanOfInt sp{new int[2], 2};
  return sp;
}

inline ConstSpanOfInt initConstSpan() {
  return ConstSpanOfInt(iarray);
}

inline SpanOfInt initSpan() {
  return SpanOfInt(iarray);
}

inline SpanOfInt initSpan(int arr[], size_t size) {
  return SpanOfInt(arr, size);
}

struct DependsOnSelf {
  std::vector<int> v;
  __attribute__((swift_name("get()")))
  ConstSpanOfInt get() const [[clang::lifetimebound]] { return ConstSpanOfInt(v.data(), v.size()); }
};

inline struct SpanBox getStructSpanBox() { return {iarray, iarray, sarray, sarray}; }

struct CaptureByReference {
    void set(const std::vector<int>& x [[clang::lifetime_capture_by(this)]]) { 
        this->x = ConstSpanOfInt(x.data(), x.size());
    };
    ConstSpanOfInt x;
};

inline void funcWithSafeWrapper(ConstSpanOfInt s [[clang::noescape]]) {}

inline ConstSpanOfInt funcWithSafeWrapper2(ConstSpanOfInt s
                                           [[clang::lifetimebound]]) {
  return s;
}

inline ConstSpanOfInt funcWithSafeWrapper3(const VecOfInt &v
                                           [[clang::lifetimebound]]) {
  return ConstSpanOfInt(v.data(), v.size());
}

struct X {
  inline void methodWithSafeWrapper(ConstSpanOfInt s [[clang::noescape]]) {}
};

#endif // TEST_INTEROP_CXX_STDLIB_INPUTS_STD_SPAN_H

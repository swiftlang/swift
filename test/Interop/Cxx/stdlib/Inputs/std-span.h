#ifndef TEST_INTEROP_CXX_STDLIB_INPUTS_STD_SPAN_H
#define TEST_INTEROP_CXX_STDLIB_INPUTS_STD_SPAN_H

#include <cstddef>
#include <span>
#include <string>
#include <vector>

#ifdef __counted_by
#undef __counted_by // some libstdc++ versions seem to include incompatible definitions of __counted_by??
#endif

#if defined(__has_feature) && __has_feature(bounds_safety_attributes)
#define __counted_by(x) __attribute__((__counted_by__(x)))
#else
#define __counted_by(x)
#endif

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
  __attribute__((swift_attr("@safe")))
  ConstSpanOfInt get() const [[clang::lifetimebound]] { return ConstSpanOfInt(v.data(), v.size()); }
};

inline struct SpanBox getStructSpanBox() { return {iarray, iarray, sarray, sarray}; }

struct CaptureByReference {
    void set(const std::vector<int>& x [[clang::lifetime_capture_by(this)]]) { 
        this->x = ConstSpanOfInt(x.data(), x.size());
    };
    ConstSpanOfInt x;
};

// expected-remark@+1{{added safe interop wrapper}}
inline void funcWithSafeWrapper(ConstSpanOfInt s [[clang::noescape]]) {}

// expected-experimental-remark@+2{{added safe interop wrapper}}
// expected-default-remark@+1{{did not add safe interop wrapper}}
inline ConstSpanOfInt funcWithSafeWrapper2(ConstSpanOfInt s
                                           // expected-default-note@+1{{lifetimebound support is not yet stabilized}}
                                           [[clang::lifetimebound]]) {
  return s;
}

// expected-experimental-remark@+2{{added safe interop wrapper}}
// expected-default-remark@+1{{did not add safe interop wrapper}}
inline ConstSpanOfInt funcWithSafeWrapper3(const VecOfInt &v
                                           // expected-default-note@+1{{lifetimebound support is not yet stabilized}}
                                           [[clang::lifetimebound]]) {
  return ConstSpanOfInt(v.data(), v.size());
}

// expected-note@+2{{implicit functions are ignored}}
// expected-remark@+1{{did not add safe interop wrapper}}
struct X {
  // expected-remark@+1{{added safe interop wrapper}}
  inline void methodWithSafeWrapper(ConstSpanOfInt s [[clang::noescape]]) {}
  // expected-remark@+1{{added safe interop wrapper}}
  SpanOfInt getMutable(ConstSpanOfInt s [[clang::noescape]]) [[clang::lifetimebound]];
};

// expected-default-note@+3{{'mixedFuncWithSafeWrapper1' declared here}}
// expected-experimental-remark@+2{{added safe interop wrapper}}
// expected-default-remark@+1{{did not add safe interop wrapper}}
inline ConstSpanOfInt mixedFuncWithSafeWrapper1(const int * __counted_by(len) p
                                           // expected-default-note@+1{{lifetimebound support is not yet stabilized}}
                                           [[clang::lifetimebound]], int len) {
  return ConstSpanOfInt(p, len);
}

// expected-experimental-remark@+2{{added safe interop wrapper}}
// expected-default-remark@+1{{did not add safe interop wrapper}}
inline const int * __counted_by(len) mixedFuncWithSafeWrapper2(const VecOfInt &v
                                           // expected-default-note@+1{{lifetimebound support is not yet stabilized}}
                                           [[clang::lifetimebound]], int len) {
  if (v.size() <= len)
    return v.data();
  return nullptr;
}

// expected-remark@+1{{added safe interop wrapper}}
inline void mixedFuncWithSafeWrapper3(ConstSpanOfInt s [[clang::noescape]],
                                      int * __counted_by(len) p, int len) {}

// expected-remark@+1{{added safe interop wrapper}}
inline void mixedFuncWithSafeWrapper4(ConstSpanOfInt s [[clang::noescape]],
                                      const int * __counted_by(len) p [[clang::noescape]], int len) {}

// expected-remark@+1{{added safe interop wrapper}}
inline void mixedFuncWithSafeWrapper5(ConstSpanOfInt s,
                                      const int * __counted_by(len) p [[clang::noescape]], int len) {}

// expected-remark@+1{{added safe interop wrapper}}
inline void mixedFuncWithSafeWrapper6(ConstSpanOfInt s,
                                      int * __counted_by(len) p, int len) {}

// expected-remark@+1{{added safe interop wrapper}}
inline ConstSpanOfInt mixedFuncWithSafeWrapper7(const int * __counted_by(len) p, int len) {
  return ConstSpanOfInt(p, len);
}

// expected-remark@+1{{added safe interop wrapper}}
inline void FuncWithMutableSafeWrapper(SpanOfInt s [[clang::noescape]]) {}

// expected-experimental-remark@+2{{added safe interop wrapper}}
// expected-default-remark@+1{{did not add safe interop wrapper}}
inline SpanOfInt FuncWithMutableSafeWrapper2(SpanOfInt s
                                           // expected-default-note@+1{{lifetimebound support is not yet stabilized}}
                                           [[clang::lifetimebound]]) {
  return s;
}

inline SpanOfInt FuncWithMutableSafeWrapper3(VecOfInt &v
                                           [[clang::lifetimebound]]) {
  return SpanOfInt(v.data(), v.size());
}

struct Y {
  inline void methodWithMutableSafeWrapper(SpanOfInt s [[clang::noescape]]) {}
};

// expected-default-note@+3{{'MixedFuncWithMutableSafeWrapper1' declared here}}
// expected-experimental-remark@+2{{added safe interop wrapper}}
// expected-default-remark@+1{{did not add safe interop wrapper}}
inline SpanOfInt MixedFuncWithMutableSafeWrapper1(int * __counted_by(len) p
                                           // expected-default-note@+1{{lifetimebound support is not yet stabilized}}
                                           [[clang::lifetimebound]], int len) {
  return SpanOfInt(p, len);
}

// expected-experimental-remark@+2{{added safe interop wrapper}}
// expected-default-remark@+1{{did not add safe interop wrapper}}
inline int * __counted_by(len) MixedFuncWithMutableSafeWrapper2(VecOfInt &v
                                           // expected-default-note@+1{{lifetimebound support is not yet stabilized}}
                                           [[clang::lifetimebound]], int len) {
  if (v.size() <= len)
    return v.data();
  return nullptr;
}

// expected-remark@+1{{added safe interop wrapper}}
inline void MixedFuncWithMutableSafeWrapper3(SpanOfInt s [[clang::noescape]],
                                      int * __counted_by(len) p, int len) {}

// expected-remark@+1{{added safe interop wrapper}}
inline void MixedFuncWithMutableSafeWrapper4(SpanOfInt s [[clang::noescape]],
                                      int * __counted_by(len) p [[clang::noescape]], int len) {}

// expected-remark@+1{{added safe interop wrapper}}
inline void MixedFuncWithMutableSafeWrapper5(SpanOfInt s,
                                      int * __counted_by(len) p [[clang::noescape]], int len) {}

// expected-remark@+1{{added safe interop wrapper}}
inline void MixedFuncWithMutableSafeWrapper6(SpanOfInt s,
                                      int * __counted_by(len) p, int len) {}

// expected-remark@+1{{added safe interop wrapper}}
inline SpanOfInt MixedFuncWithMutableSafeWrapper7(int * __counted_by(len) p, int len) {
  return SpanOfInt(p, len);
}

template <typename X>
// expected-remark@+2{{did not add safe interop wrapper}}
// expected-note@+1{{implicit functions are ignored}}
struct S {};

// expected-remark@+2{{did not add safe interop wrapper}}
// expected-note@+1{{implicit functions are ignored}}
struct SpanWithoutTypeAlias {
  // expected-remark@+2{{did not add safe interop wrapper}}
  // expected-note@+1{{template specialization cannot be represented in Swift syntax; try hiding it behind a typedef}}
  std::span<const int> bar() [[clang::lifetimebound]];
  // expected-note@+2{{template specialization cannot be represented in Swift syntax; try hiding it behind a typedef}}
  // expected-remark@+1{{did not add safe interop wrapper}}
  void foo(std::span<const int> s [[clang::noescape]]);
  // expected-note@+2{{template specialization cannot be represented in Swift syntax; try hiding it behind a typedef}}
  // expected-remark@+1{{did not add safe interop wrapper}}
  void otherTemplatedType(ConstSpanOfInt copy [[clang::noescape]], S<int>);
  // expected-note@+2{{template specialization cannot be represented in Swift syntax; try hiding it behind a typedef}}
  // expected-remark@+1{{did not add safe interop wrapper}}
  void otherTemplatedType2(ConstSpanOfInt copy [[clang::noescape]], S<int> *);
};

inline void func(ConstSpanOfInt copy [[clang::noescape]]) {}
// expected-remark@+1{{added safe interop wrapper}}
inline void mutableKeyword(SpanOfInt copy [[clang::noescape]]) {}

// expected-note@+2{{template specialization cannot be represented in Swift syntax; try hiding it behind a typedef}}
// expected-remark@+1{{did not add safe interop wrapper}}
inline void spanWithoutTypeAlias(std::span<const int> s [[clang::noescape]]) {}
// expected-note@+2{{template specialization cannot be represented in Swift syntax; try hiding it behind a typedef}}
// expected-remark@+1{{did not add safe interop wrapper}}
inline void mutableSpanWithoutTypeAlias(std::span<int> s [[clang::noescape]]) {}

#define IMMORTAL_FRT                                                           \
  __attribute__((swift_attr("import_reference")))                              \
  __attribute__((swift_attr("retain:immortal")))                               \
  __attribute__((swift_attr("release:immortal")))

struct IMMORTAL_FRT DependsOnSelfFRT {
  std::vector<int> v;
  __attribute__((swift_name("get()"))) ConstSpanOfInt get() const
      [[clang::lifetimebound]] {
    return ConstSpanOfInt(v.data(), v.size());
  }
  SpanOfInt getMutable() [[clang::lifetimebound]] {
    return SpanOfInt(v.data(), v.size());
  }
};

struct NonCopyable {
  NonCopyable(int n) : number(n) {}
  NonCopyable(const NonCopyable &other) = delete;
  NonCopyable(NonCopyable &&other) = default;
  ~NonCopyable() {}
  int number;
};

using SpanOfNonCopyable = std::span<NonCopyable>;

inline SpanOfNonCopyable makeSpanOfNonCopyable() {
  static NonCopyable arr[]{1, 2, 3};
  return SpanOfNonCopyable(arr);
}

#endif // TEST_INTEROP_CXX_STDLIB_INPUTS_STD_SPAN_H

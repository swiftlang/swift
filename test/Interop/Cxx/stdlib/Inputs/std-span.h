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

// CHECK:     struct DependsOnSelf {
// CHECK:       @safe borrowing func get() -> ConstSpanOfInt
// CHECK-LEGACY:       @_lifetime(borrow self)
// CHECK-LEGACY-NEXT:  @_alwaysEmitIntoClient @_disfavoredOverload public borrowing func get() -> Span<CInt>
// CHECK:     }

inline struct SpanBox getStructSpanBox() { return {iarray, iarray, sarray, sarray}; }

struct CaptureByReference {
    void set(const std::vector<int>& x [[clang::lifetime_capture_by(this)]]) { 
        this->x = ConstSpanOfInt(x.data(), x.size());
    };
    ConstSpanOfInt x;
};

// CHECK:     struct CaptureByReference {
// CHECK:      mutating func set(_ x: borrowing std.{{.*}}vector<CInt, std.{{.*}}allocator<CInt>>)
// CHECK:     }

inline void funcWithSafeWrapper(ConstSpanOfInt s [[clang::noescape]]) {}

// CHECK:      func funcWithSafeWrapper(_ s: ConstSpanOfInt)
// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func funcWithSafeWrapper(_ s: Span<CInt>)

inline ConstSpanOfInt funcWithSafeWrapper2(ConstSpanOfInt s
                                           [[clang::lifetimebound]]) {
  return s;
}

// CHECK:      func funcWithSafeWrapper2(_ s: ConstSpanOfInt) -> ConstSpanOfInt
// CHECK-LEGACY-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-LEGACY-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-LEGACY-NEXT: @_lifetime(copy s)
// CHECK-LEGACY-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func funcWithSafeWrapper2(_ s: Span<CInt>) -> Span<CInt>

inline ConstSpanOfInt funcWithSafeWrapper3(const VecOfInt &v
                                           [[clang::lifetimebound]]) {
  return ConstSpanOfInt(v.data(), v.size());
}

// CHECK:      func funcWithSafeWrapper3(_ v: borrowing VecOfInt) -> ConstSpanOfInt
// CHECK-LEGACY-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-LEGACY-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-LEGACY-NEXT: @_lifetime(borrow v)
// CHECK-LEGACY-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func funcWithSafeWrapper3(_ v: borrowing VecOfInt) -> Span<CInt>

struct X {
  inline void methodWithSafeWrapper(ConstSpanOfInt s [[clang::noescape]]) {}
  SpanOfInt getMutable(ConstSpanOfInt s [[clang::noescape]]) [[clang::lifetimebound]];
};

// CHECK:      struct X {
// CHECK-NEXT:   init()
// CHECK-NEXT:   mutating func methodWithSafeWrapper(_ s: ConstSpanOfInt)
// CHECK-NEXT:   /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:   @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT:   @_alwaysEmitIntoClient @_disfavoredOverload public mutating func methodWithSafeWrapper(_ s: Span<CInt>)
// CHECK-NEXT:   mutating func getMutable(_ s: ConstSpanOfInt) -> SpanOfInt
// CHECK-LEGACY-NEXT:   /// This is an auto-generated wrapper for safer interop
// CHECK-LEGACY-NEXT:   @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-LEGACY-NEXT:   @_lifetime(&self)
// CHECK-LEGACY-NEXT:   @_alwaysEmitIntoClient @_disfavoredOverload public mutating func getMutable(_ s: Span<CInt>) -> MutableSpan<CInt>
// CHECK-NEXT: }

inline ConstSpanOfInt mixedFuncWithSafeWrapper1(const int * __counted_by(len) p
                                           [[clang::lifetimebound]], int len) {
  return ConstSpanOfInt(p, len);
}

// CHECK:      func mixedFuncWithSafeWrapper1(_ p: UnsafePointer<CInt>!, _ len: CInt) -> ConstSpanOfInt
// CHECK-LEGACY-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-LEGACY-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-LEGACY-NEXT: @_lifetime(copy p)
// CHECK-LEGACY-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func mixedFuncWithSafeWrapper1(_ p: Span<CInt>) -> Span<CInt>

inline const int * __counted_by(len) mixedFuncWithSafeWrapper2(const VecOfInt &v
                                           [[clang::lifetimebound]], int len) {
  if (v.size() <= len)
    return v.data();
  return nullptr;
}

// CHECK:      func mixedFuncWithSafeWrapper2(_ v: borrowing VecOfInt, _ len: CInt) -> UnsafePointer<CInt>!
// CHECK-LEGACY-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-LEGACY-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-LEGACY-NEXT: @_lifetime(borrow v)
// CHECK-LEGACY-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func mixedFuncWithSafeWrapper2(_ v: borrowing VecOfInt, _ len: CInt) -> Span<CInt>

inline void mixedFuncWithSafeWrapper3(ConstSpanOfInt s [[clang::noescape]],
                                      int * __counted_by(len) p, int len) {}

// CHECK:      func mixedFuncWithSafeWrapper3(_ s: ConstSpanOfInt, _ p: UnsafeMutablePointer<CInt>!, _ len: CInt)
// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func mixedFuncWithSafeWrapper3(_ s: Span<CInt>, _ p: UnsafeMutableBufferPointer<CInt>)

inline void mixedFuncWithSafeWrapper4(ConstSpanOfInt s [[clang::noescape]],
                                      const int * __counted_by(len) p [[clang::noescape]], int len) {}

// CHECK:      func mixedFuncWithSafeWrapper4(_ s: ConstSpanOfInt, _ p: UnsafePointer<CInt>!, _ len: CInt)
// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func mixedFuncWithSafeWrapper4(_ s: Span<CInt>, _ p: Span<CInt>)

inline void mixedFuncWithSafeWrapper5(ConstSpanOfInt s,
                                      const int * __counted_by(len) p [[clang::noescape]], int len) {}

// CHECK:      func mixedFuncWithSafeWrapper5(_ s: ConstSpanOfInt, _ p: UnsafePointer<CInt>!, _ len: CInt)
// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func mixedFuncWithSafeWrapper5(_ s: ConstSpanOfInt, _ p: Span<CInt>)

inline void mixedFuncWithSafeWrapper6(ConstSpanOfInt s,
                                      int * __counted_by(len) p, int len) {}

// CHECK:      func mixedFuncWithSafeWrapper6(_ s: ConstSpanOfInt, _ p: UnsafeMutablePointer<CInt>!, _ len: CInt)
// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func mixedFuncWithSafeWrapper6(_ s: ConstSpanOfInt, _ p: UnsafeMutableBufferPointer<CInt>)

inline ConstSpanOfInt mixedFuncWithSafeWrapper7(const int * __counted_by(len) p, int len) {
  return ConstSpanOfInt(p, len);
}

// CHECK:      func mixedFuncWithSafeWrapper7(_ p: UnsafePointer<CInt>!, _ len: CInt) -> ConstSpanOfInt
// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func mixedFuncWithSafeWrapper7(_ p: UnsafeBufferPointer<CInt>) -> ConstSpanOfInt

inline void FuncWithMutableSafeWrapper(SpanOfInt s [[clang::noescape]]) {}

// CHECK:      func FuncWithMutableSafeWrapper(_ s: SpanOfInt)
// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_lifetime(s: copy s)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func FuncWithMutableSafeWrapper(_ s: inout MutableSpan<CInt>)

inline SpanOfInt FuncWithMutableSafeWrapper2(SpanOfInt s
                                           [[clang::lifetimebound]]) {
  return s;
}

// CHECK:      func FuncWithMutableSafeWrapper2(_ s: SpanOfInt) -> SpanOfInt
// CHECK-LEGACY-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-LEGACY-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-LEGACY-NEXT: @_lifetime(copy s)
// CHECK-LEGACY-NEXT: @_lifetime(s: copy s)
// CHECK-LEGACY-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func FuncWithMutableSafeWrapper2(_ s: inout MutableSpan<CInt>) -> MutableSpan<CInt>

inline SpanOfInt FuncWithMutableSafeWrapper3(VecOfInt &v
                                           [[clang::lifetimebound]]) {
  return SpanOfInt(v.data(), v.size());
}

// CHECK:      func FuncWithMutableSafeWrapper3(_ v: inout VecOfInt) -> SpanOfInt
// CHECK-LEGACY-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-LEGACY-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-LEGACY-NEXT: @_lifetime(&v)
// CHECK-LEGACY-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func FuncWithMutableSafeWrapper3(_ v: inout VecOfInt) -> MutableSpan<CInt>

struct Y {
  inline void methodWithMutableSafeWrapper(SpanOfInt s [[clang::noescape]]) {}
};

// CHECK:      mutating func methodWithMutableSafeWrapper(_ s: SpanOfInt)
// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_lifetime(s: copy s)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public mutating func methodWithMutableSafeWrapper(_ s: inout MutableSpan<CInt>)

inline SpanOfInt MixedFuncWithMutableSafeWrapper1(int * __counted_by(len) p
                                           [[clang::lifetimebound]], int len) {
  return SpanOfInt(p, len);
}

// CHECK:      func MixedFuncWithMutableSafeWrapper1(_ p: UnsafeMutablePointer<CInt>!, _ len: CInt) -> SpanOfInt
// CHECK-LEGACY-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-LEGACY-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-LEGACY-NEXT: @_lifetime(copy p)
// CHECK-LEGACY-NEXT: @_lifetime(p: copy p)
// CHECK-LEGACY-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func MixedFuncWithMutableSafeWrapper1(_ p: inout MutableSpan<CInt>) -> MutableSpan<CInt>

inline int * __counted_by(len) MixedFuncWithMutableSafeWrapper2(VecOfInt &v
                                           [[clang::lifetimebound]], int len) {
  if (v.size() <= len)
    return v.data();
  return nullptr;
}

// CHECK:      func MixedFuncWithMutableSafeWrapper2(_ v: inout VecOfInt, _ len: CInt) -> UnsafeMutablePointer<CInt>!
// CHECK-LEGACY-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-LEGACY-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-LEGACY-NEXT: @_lifetime(&v)
// CHECK-LEGACY-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func MixedFuncWithMutableSafeWrapper2(_ v: inout VecOfInt, _ len: CInt) -> MutableSpan<CInt>

inline void MixedFuncWithMutableSafeWrapper3(SpanOfInt s [[clang::noescape]],
                                      int * __counted_by(len) p, int len) {}

// CHECK:      func MixedFuncWithMutableSafeWrapper3(_ s: SpanOfInt, _ p: UnsafeMutablePointer<CInt>!, _ len: CInt)
// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_lifetime(s: copy s)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func MixedFuncWithMutableSafeWrapper3(_ s: inout MutableSpan<CInt>, _ p: UnsafeMutableBufferPointer<CInt>)

inline void MixedFuncWithMutableSafeWrapper4(SpanOfInt s [[clang::noescape]],
                                      int * __counted_by(len) p [[clang::noescape]], int len) {}

// CHECK:      func MixedFuncWithMutableSafeWrapper4(_ s: SpanOfInt, _ p: UnsafeMutablePointer<CInt>!, _ len: CInt)
// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_lifetime(s: copy s)
// CHECK-NEXT: @_lifetime(p: copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func MixedFuncWithMutableSafeWrapper4(_ s: inout MutableSpan<CInt>, _ p: inout MutableSpan<CInt>)

inline void MixedFuncWithMutableSafeWrapper5(SpanOfInt s,
                                      int * __counted_by(len) p [[clang::noescape]], int len) {}

// CHECK:      func MixedFuncWithMutableSafeWrapper5(_ s: SpanOfInt, _ p: UnsafeMutablePointer<CInt>!, _ len: CInt)
// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_lifetime(p: copy p)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func MixedFuncWithMutableSafeWrapper5(_ s: SpanOfInt, _ p: inout MutableSpan<CInt>)

inline void MixedFuncWithMutableSafeWrapper6(SpanOfInt s,
                                      int * __counted_by(len) p, int len) {}

// CHECK:      func MixedFuncWithMutableSafeWrapper6(_ s: SpanOfInt, _ p: UnsafeMutablePointer<CInt>!, _ len: CInt)
// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func MixedFuncWithMutableSafeWrapper6(_ s: SpanOfInt, _ p: UnsafeMutableBufferPointer<CInt>)

inline SpanOfInt MixedFuncWithMutableSafeWrapper7(int * __counted_by(len) p, int len) {
  return SpanOfInt(p, len);
}

// CHECK:      func MixedFuncWithMutableSafeWrapper7(_ p: UnsafeMutablePointer<CInt>!, _ len: CInt) -> SpanOfInt
// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func MixedFuncWithMutableSafeWrapper7(_ p: UnsafeMutableBufferPointer<CInt>) -> SpanOfInt

template <typename X>
struct S {};

struct SpanWithoutTypeAlias {
  std::span<const int> bar() [[clang::lifetimebound]];
  void foo(std::span<const int> s [[clang::noescape]]);
  std::span<S<int>> nestedTemplateInstantiationReturn() [[clang::lifetimebound]];
  void nestedTemplateInstantiationParam(std::span<S<int>> s [[clang::noescape]]);
  S<int> * __counted_by(len) nestedTemplateInstantiationReturnCounted(int len) [[clang::lifetimebound]];
  void nestedTemplateInstantiationParamCounted(S<int> * __counted_by(len) p [[clang::noescape]], int len);
  void otherTemplatedType(ConstSpanOfInt copy [[clang::noescape]], S<int>);
  void otherTemplatedType2(ConstSpanOfInt copy [[clang::noescape]], S<int> *);
  S<int> *otherTemplatedTypeReturn(ConstSpanOfInt copy [[clang::noescape]]);
};

// CHECK: struct SpanWithoutTypeAlias {
// CHECK-NEXT:   init()
// CHECK-NEXT:   mutating func bar() -> std.{{.*}}span<__cxxConst<CInt>, _C{{.*}}_{{.*}}>
// CHECK-LEGACY-NEXT:   /// This is an auto-generated wrapper for safer interop
// CHECK-LEGACY-NEXT:   @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-LEGACY-NEXT:   @_lifetime(&self)
// CHECK-LEGACY-NEXT:   @_alwaysEmitIntoClient @_disfavoredOverload public mutating func bar() -> Span<CInt>
// CHECK-NEXT:   mutating func foo(_ s: std.{{.*}}span<__cxxConst<CInt>, _C{{.*}}_{{.*}}>)
// CHECK-NEXT:   /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:   @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT:   @_alwaysEmitIntoClient @_disfavoredOverload public mutating func foo(_ s: Span<CInt>)
// CHECK-NEXT:   mutating func nestedTemplateInstantiationReturn() -> std.{{.*}}span<S<CInt>, _C{{.*}}_{{.*}}>
// CHECK-NEXT:   mutating func nestedTemplateInstantiationParam(_ s: std.{{.*}}span<S<CInt>, _C{{.*}}_{{.*}}>)
// CHECK-NEXT:   mutating func nestedTemplateInstantiationReturnCounted(_ len: CInt) -> UnsafeMutablePointer<S<CInt>>!
// CHECK-NEXT:   mutating func nestedTemplateInstantiationParamCounted(_ p: UnsafeMutablePointer<S<CInt>>!, _ len: CInt)
// CHECK-NEXT:   mutating func otherTemplatedType(_ copy: ConstSpanOfInt, _: S<CInt>)
// CHECK-NEXT:   mutating func otherTemplatedType2(_ copy: ConstSpanOfInt, _: UnsafeMutablePointer<S<CInt>>!)
// CHECK-NEXT:   mutating func otherTemplatedTypeReturn(_ copy: ConstSpanOfInt) -> UnsafeMutablePointer<S<CInt>>!
// CHECK-NEXT: }

inline void func(ConstSpanOfInt copy [[clang::noescape]]) {}

// CHECK:      func `func`(_ copy: ConstSpanOfInt)
// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func `func`(_ copy: Span<CInt>)
inline void mutableKeyword(SpanOfInt copy [[clang::noescape]]) {}

// CHECK:      func mutableKeyword(_ copy: SpanOfInt)
// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_lifetime(copy: copy copy)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func mutableKeyword(_ copy: inout MutableSpan<CInt>)

inline void spanWithoutTypeAlias(std::span<const int> s [[clang::noescape]]) {}

// CHECK:      func spanWithoutTypeAlias(_ s: std.{{.*}}span<__cxxConst<CInt>, _C{{.*}}_{{.*}}>)
// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func spanWithoutTypeAlias(_ s: Span<CInt>)
inline void mutableSpanWithoutTypeAlias(std::span<int> s [[clang::noescape]]) {}

// CHECK:      func mutableSpanWithoutTypeAlias(_ s: std.{{.*}}span<CInt, _C{{.*}}_{{.*}}>)
// CHECK-NEXT: /// This is an auto-generated wrapper for safer interop
// CHECK-NEXT: @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// CHECK-NEXT: @_lifetime(s: copy s)
// CHECK-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func mutableSpanWithoutTypeAlias(_ s: inout MutableSpan<CInt>)

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

// CHECK: class DependsOnSelfFRT {
// CHECK-NEXT:   init()
// CHECK-NEXT:   var v: std.{{.*}}vector<CInt, std.{{.*}}allocator<CInt>>
// CHECK-NEXT:   borrowing func get() -> ConstSpanOfInt
// CHECK-NEXT:   borrowing func {{(__)?}}getMutable{{(Unsafe)?}}() -> SpanOfInt
// CHECK-NEXT: }

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

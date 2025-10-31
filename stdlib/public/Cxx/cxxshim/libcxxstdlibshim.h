#include <chrono>
#include <functional>
#include <string>
#include <type_traits>
//#include <ptrauth.h>

#include "libcxxshim.h"

/// Used for std::string conformance to Swift.Hashable
typedef std::hash<std::string> __swift_interopHashOfString;
inline std::size_t __swift_interopComputeHashOfString(const std::string &str) {
  return __swift_interopHashOfString()(str);
}

/// Used for std::u16string conformance to Swift.Hashable
typedef std::hash<std::u16string> __swift_interopHashOfU16String;
inline std::size_t __swift_interopComputeHashOfU16String(const std::u16string &str) {
  return __swift_interopHashOfU16String()(str);
}

/// Used for std::u32string conformance to Swift.Hashable
typedef std::hash<std::u32string> __swift_interopHashOfU32String;
inline std::size_t __swift_interopComputeHashOfU32String(const std::u32string &str) {
  return __swift_interopHashOfU32String()(str);
}

/// Used for std::wstring conformance to Swift.Hashable
typedef std::hash<std::wstring> __swift_interopHashOfWString;
inline std::size_t __swift_interopComputeHashOfWString(const std::wstring &str) {
  return __swift_interopHashOfWString()(str);
}

inline std::chrono::seconds __swift_interopMakeChronoSeconds(int64_t seconds) {
  return std::chrono::seconds(seconds);
}

inline std::chrono::milliseconds __swift_interopMakeChronoMilliseconds(int64_t milliseconds) {
  return std::chrono::milliseconds(milliseconds);
}

inline std::chrono::microseconds __swift_interopMakeChronoMicroseconds(int64_t microseconds) {
  return std::chrono::microseconds(microseconds);
}

inline std::chrono::nanoseconds __swift_interopMakeChronoNanoseconds(int64_t nanoseconds) {
  return std::chrono::nanoseconds(nanoseconds);
}

template <unsigned PtrAuthTypeDiscriminator, typename Result, typename... Args>
struct __SwiftLoweredFunctionBase {
  using LoweredFunction = void
      __attribute__((swiftcall)) (Result *__attribute__((swift_indirect_result)),
                                  std::remove_reference_t<Args> *...,
                                  void *__attribute__((swift_context)));

  __swift_interop_closure closure;

  Result operator()(Args... args) const {
    Result result;
    LoweredFunction *callable = (LoweredFunction *)closure.func;
    callable(&result, &args..., closure.context);
    return result;
  }
};

/// Swift closures that return void have a different corresponding C++ function
/// type. This partial specialization handles it.
template <unsigned PtrAuthTypeDiscriminator, typename... Args>
struct __SwiftLoweredFunctionBase<PtrAuthTypeDiscriminator, void, Args...> {
  using LoweredFunction = void
      __attribute__((swiftcall)) (std::remove_reference_t<Args> *...,
                                  void *__attribute__((swift_context)));

  __swift_interop_closure closure;

  void operator()(Args... args) const {
    LoweredFunction *callable = (LoweredFunction *)closure.func;
    callable(&args..., closure.context);
  }
};

template <unsigned PtrAuthTypeDiscriminator, typename Result, typename... Args>
struct __SwiftFunctionWrapper : __SwiftLoweredFunctionBase<PtrAuthTypeDiscriminator, Result, Args...> {
  __SwiftFunctionWrapper(const __swift_interop_closure &closure) {
    this->closure = closure;
  }

  __SwiftFunctionWrapper() = delete;
  __SwiftFunctionWrapper &operator=(const __SwiftFunctionWrapper &other) = delete;
  __SwiftFunctionWrapper &operator=(__SwiftFunctionWrapper &&other) = delete;

  __SwiftFunctionWrapper(const __SwiftFunctionWrapper &other) noexcept {
    this->closure = other.closure;
    this->closure.retain();
  }

  __SwiftFunctionWrapper(__SwiftFunctionWrapper &&other) noexcept {
    this->closure = other.closure;
    this->closure.retain();
  }

  ~__SwiftFunctionWrapper() {
    this->closure.release();
  }
};

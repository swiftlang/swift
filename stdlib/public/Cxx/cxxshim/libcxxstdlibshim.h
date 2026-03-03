#include <chrono>
#include <functional>
#include <string>
#include <type_traits>
#if __has_include(<ptrauth.h>)
#include <ptrauth.h>
#endif

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

namespace __swift_interop {
extern "C" {
void swift_retain(void *_Nonnull) noexcept;
void swift_release(void *_Nonnull) noexcept;
}
} // namespace __swift_interop

struct __swift_interop_closure {
  void *_Nonnull func;
  void *_Nullable context;
};

template <uint16_t PtrAuthTypeDiscriminator, typename Result, typename... Args>
struct __SwiftFunctionWrapper {
  /// C++ function type that is equivalent to the lowered Swift closure type.
  /// Note that Clang might pass the parameters or the return value indirectly.
  using LoweredFunction = Result
      __attribute__((swiftcall)) (Args...,
                                  void *_Nullable __attribute__((swift_context)));

  /// Swift will pretend that the type of this field is a Swift closure type.
  __swift_interop_closure closure;

  Result operator()(Args... args) const {
#if __has_include(<ptrauth.h>)
    return ((LoweredFunction *)ptrauth_auth_and_resign(
        closure.func, ptrauth_key_asia, PtrAuthTypeDiscriminator,
        ptrauth_key_function_pointer, 0))(std::forward<Args>(args)...,
                                          closure.context);
#else
    // Android NDK 28 does not define the ptrauth macros.
    return ((LoweredFunction *)closure.func)(std::forward<Args>(args)...,
                                             closure.context);
#endif
  }

  // A memberwise constructor is synthesized by Swift.

  __SwiftFunctionWrapper() = delete;
  __SwiftFunctionWrapper &operator=(const __SwiftFunctionWrapper &other) = delete;
  __SwiftFunctionWrapper &operator=(__SwiftFunctionWrapper &&other) = delete;

  __SwiftFunctionWrapper(const __SwiftFunctionWrapper &other) noexcept {
    closure = other.closure;
    if (closure.context)
      __swift_interop::swift_retain(closure.context);
  }

  __SwiftFunctionWrapper(__SwiftFunctionWrapper &&other) noexcept {
    closure = other.closure;
    other.closure.func = nullptr;
    other.closure.context = nullptr;
  }

  ~__SwiftFunctionWrapper() {
    if (closure.context)
      __swift_interop::swift_release(closure.context);
  }
};

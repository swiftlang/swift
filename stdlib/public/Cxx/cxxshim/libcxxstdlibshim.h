#include <chrono>
#include <functional>
#include <string>

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

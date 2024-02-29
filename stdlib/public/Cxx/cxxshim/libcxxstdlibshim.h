#include <chrono>
#include <functional>
#include <string>

/// Used for std::string conformance to Swift.Hashable
typedef std::hash<std::string> __swift_interopHashOfString;

/// Used for std::u16string conformance to Swift.Hashable
typedef std::hash<std::u16string> __swift_interopHashOfU16String;

/// Used for std::u32string conformance to Swift.Hashable
typedef std::hash<std::u32string> __swift_interopHashOfU32String;

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

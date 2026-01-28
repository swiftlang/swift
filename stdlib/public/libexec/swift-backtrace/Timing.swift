#if canImport(Darwin)
import Darwin
#elseif canImport(Glibc)
import Glibc
#elseif canImport(Musl)
import Musl
#elseif os(Windows)
import WinSDK
#endif

struct Timestamp: RawRepresentable, CustomStringConvertible {
  #if os(Windows)
  typealias RawValue = FILETIME
  #else
  typealias RawValue = timespec
  #endif

  var rawValue: RawValue

  init() {
    #if os(Windows)
    self.rawValue = FILETIME(dwLowDateTime: 0, dwHighDateTime: 0)
    #else
    self.rawValue = timespec(tv_sec: 0, tv_nsec: 0)
    #endif
  }

  init(rawValue: RawValue) {
    self.rawValue = rawValue
  }

  static var now: Timestamp {
    #if os(Windows)
    var ft = FILETIME()
    GetSystemTimePreciseAsFileTime(&ft)
    return Timestamp(rawValue: ft)
    #else
    var ts = timespec(tv_sec: 0, tv_nsec: 0)
    if clock_gettime(CLOCK_MONOTONIC, &ts) != 0 {
      ts.tv_sec = time(nil)
      ts.tv_nsec = 0
    }
    return Timestamp(rawValue: ts)
    #endif
  }

  static var wallTime: Timestamp {
    #if os(Windows)
    var ft = FILETIME()
    GetSystemTimePreciseAsFileTime(&ft)
    return Timestamp(rawValue: ft)
    #else
    var ts = timespec(tv_sec: 0, tv_nsec: 0)
    if clock_gettime(CLOCK_REALTIME, &ts) != 0 {
      ts.tv_sec = time(nil)
      ts.tv_nsec = 0
    }
    return Timestamp(rawValue: ts)
    #endif
  }

  #if os(Windows)
  var flatTimestamp: UInt64 {
    return (UInt64(rawValue.dwLowDateTime)
              | (UInt64(rawValue.dwHighDateTime) << 32))
  }
  #endif

  var iso8601: String {
    var exploded = tm()
    #if os(Windows)
    var secs = time_t(flatTimestamp / 10000000)
    let nanos = 100 * (flatTimestamp % 10000000)

    gmtime_s(&exploded, &secs)
    #else
    var secs = rawValue.tv_sec
    let nanos = rawValue.tv_nsec

    gmtime_r(&secs, &exploded)
    #endif

    let isoTime = """
\(String(exploded.tm_year + 1900, width: 4))-\
\(String(exploded.tm_mon + 1, width: 2))-\
\(String(exploded.tm_mday, width: 2))T\
\(String(exploded.tm_hour, width: 2)):\
\(String(exploded.tm_min, width: 2)):\
\(String(exploded.tm_sec, width: 2)).\
\(String(nanos / 1000, width: 6))Z
"""

    return isoTime
  }

  var description: String {
    #if os(Windows)
    let flat = flatTimestamp
    let nanos = 100 * (flat % 10000000)
    let secs = flat / 10000000
    #else
    let nanos = rawValue.tv_nsec
    let secs = rawValue.tv_sec
    #endif

    let digits = String(nanos)
    let padding = String(repeating: "0", count: 9 - digits.count)
    return "\(secs).\(padding)\(digits)"
  }
}

struct Duration: RawRepresentable, CustomStringConvertible {
  #if os(Windows)
  typealias RawValue = FILETIME
  #else
  typealias RawValue = timespec
  #endif

  var rawValue: RawValue

  init() {
    #if os(Windows)
    self.rawValue = FILETIME(dwLowDateTime: 0, dwHighDateTime: 0)
    #else
    self.rawValue = timespec(tv_sec: 0, tv_nsec: 0)
    #endif
  }

  init(rawValue: RawValue) {
    self.rawValue = rawValue
  }

  #if os(Windows)
  var flatDuration: UInt64 {
    return (UInt64(rawValue.dwLowDateTime)
              | (UInt64(rawValue.dwHighDateTime) << 32))
  }
  #endif

  var description: String {
    #if os(Windows)
    let flat = flatDuration
    let centisRounded = (flat + 50000) / 100000
    let centis = centisRounded % 100
    let secs = (flat + 50000) / 10000000
    #else
    let centisRounded = (rawValue.tv_nsec + 5000000) / 10000000
    let centis = centisRounded % 100
    let secs = rawValue.tv_sec + (centisRounded / 100)
    #endif

    let d1 = centis / 10
    let d2 = centis % 10

    return "\(secs).\(d1)\(d2)"
  }

  static func measure(_ body: () -> ()) -> Duration {
    let start = Timestamp.now

    body()

    let end = Timestamp.now

    return end - start
  }
}

extension Double {
  init(duration: Duration) {
    #if os(Windows)
    let flat = duration.flatDuration
    self = Double(flat) * 100e-9
    #else
    self = Double(duration.rawValue.tv_sec)
      + 1e-9 * Double(duration.rawValue.tv_nsec)
    #endif
  }
}

func -(lhs: Timestamp, rhs: Timestamp) -> Duration {
  #if os(Windows)
  let flatLhs = lhs.flatTimestamp
  let flatRhs = rhs.flatTimestamp

  assert(flatLhs >= flatRhs, "Durations cannot be negative")

  let flatDuration = flatLhs - flatRhs
  let rawDuration = FILETIME(
    dwLowDateTime: UInt32(truncatingIfNeeded: flatDuration),
    dwHighDateTime: UInt32(truncatingIfNeeded: flatDuration >> 32)
  )
  return Duration(rawValue: rawDuration)
  #else
  let nanos = lhs.rawValue.tv_nsec - rhs.rawValue.tv_nsec
  let adjustedNanos = nanos >= 0 ? nanos : nanos + 1000000000
  let secs = lhs.rawValue.tv_sec - rhs.rawValue.tv_sec
  let adjustedSecs = secs - (nanos >= 0 ? 0 : 1)

  let rawDuration = timespec(tv_sec: adjustedSecs,
                             tv_nsec: adjustedNanos)
  return Duration(rawValue: rawDuration)
  #endif
}

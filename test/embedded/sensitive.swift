// RUN: %target-run-simple-swift(   -parse-as-library -enable-experimental-feature Sensitive -enable-experimental-feature Embedded -wmo -Xfrontend -disable-access-control -runtime-compatibility-version none)
// RUN: %target-run-simple-swift(-O -parse-as-library -enable-experimental-feature Sensitive -enable-experimental-feature Embedded -wmo -Xfrontend -disable-access-control -runtime-compatibility-version none)
// RUN: %target-run-simple-swift(-target %module-target-future -parse-as-library -enable-experimental-feature Sensitive -enable-experimental-feature Embedded -wmo -Xfrontend -disable-access-control -runtime-compatibility-version none)

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_Sensitive

var checkBuffer: UnsafeBufferPointer<UInt32>?

@inline(never)
func checkLater<T>(_ t: inout T) {
  withUnsafePointer(to: &t) {
    let size = MemoryLayout<T>.size / MemoryLayout<UInt32>.size
    $0.withMemoryRebound(to: UInt32.self, capacity: size) {
      checkBuffer = UnsafeBufferPointer(start: $0, count: size)
    }
  }
}

@inline(never)
func check() {
  for b in checkBuffer! {
    precondition(b != 0xdeadbeaf)
  }
}

@inline(never)
func testSensitive<T>(_ t: T) {
  do {
    var x: T = t
    checkLater(&x)
  }
  check()
  print(0) // to prevent tail call of `check()`
}

@sensitive
struct SensitiveStruct {
  var a = 0xdeadbeaf
  var b = 0xdeadbeaf
  var c = 0xdeadbeaf
}

struct Container<T> {
  var x = 123
  let t: T
  var y = 456
}

@main struct Main {
  static func main() {
    testSensitive(SensitiveStruct())
    testSensitive(Optional(SensitiveStruct()))
    testSensitive(Container(t: SensitiveStruct()))
  }
}


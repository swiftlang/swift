// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -wmo) | %FileCheck %s

// REQUIRES: executable_test
// This test requires stdlib function that are currently only available in WASI-libc.
// REQUIRES: OS=wasip1
// REQUIRES: swift_feature_Embedded

enum E: Error {
  case foo
}

@main struct Main {
  static func main() {
    do throws(E) {
      try "Hello".withCString { (pointer) throws(E) in
        var i = 0
        var pointer = pointer
        while pointer.pointee != 0 {
          i += 1
          pointer += 1
        }
        print(i) // CHECK: 5
        throw E.foo
      }
    } catch {
      print("error caught") // CHECK: error caught
    }
  }
}

// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -enable-experimental-feature EmbeddedExistentials -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedExistentials -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu || OS=wasip1
// REQUIRES: swift_feature_Embedded

enum MyError: Error, Equatable {
  case simple
  case withValue(Int)
}

func mayThrow(_ n: Int) throws {
  if n == 0 { throw MyError.simple }
  if n == 1 { throw MyError.withValue(42) }
}

@main
struct Main {
  static func main() {
    // Catch untyped error, verify it's the right case
    do {
      try mayThrow(0)
    } catch let e as MyError {
      print(e == .simple ? "OK1" : "FAIL")
    } catch {
      print("FAIL")
    }

    do {
      try mayThrow(1)
    } catch let e as MyError {
      if case .withValue(let n) = e {
        print(n == 42 ? "OK2" : "FAIL")
      } else {
        print("FAIL")
      }
    } catch {
      print("FAIL")
    }

    // No-throw path
    do {
      try mayThrow(99)
      print("OK3")
    } catch {
      print("FAIL")
    }
  }
}

// CHECK: OK1
// CHECK: OK2
// CHECK: OK3
// CHECK-NOT: FAIL

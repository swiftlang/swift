// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -runtime-compatibility-version none -wmo %target-embedded-posix-shim) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

public enum MyError : Error {
  case a
  case b
  case c

  public var _domain: String {
    "My Error Domain"
  }
}

public struct MyOtherError: Error {
  public var _code: Int {
    12345
  }

  public var _domain: String {
    "My Other Domain"
  }
}

func `throw`(_ error: some Error) throws(any Error) {
  throw error
}

func `catch`(_ error: some Error) {
  print(error._code)
  print(error._domain)
}

func throwAndCatch(_ error: some Error) {
  do {
    try `throw`(error)
  } catch {
    `catch`(error)
  }
}

func throwThemAll() {
  throwAndCatch(MyError.a)
  throwAndCatch(MyError.b)
  throwAndCatch(MyError.c)
  throwAndCatch(MyOtherError())
}

@main
struct Main {
  static func main() {
    throwThemAll()
  }
}

// Without type metadata the default `_code` of an enum can't use the case
// index, so it is always 1.
// CHECK: 1
// CHECK-NEXT: My Error Domain
// CHECK-NEXT: 1
// CHECK-NEXT: My Error Domain
// CHECK-NEXT: 1
// CHECK-NEXT: My Error Domain
// CHECK-NEXT: 12345
// CHECK-NEXT: My Other Domain

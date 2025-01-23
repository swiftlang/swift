// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -enable-experimental-feature Embedded -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s
// RUN: %target-run-simple-swift(-Osize -enable-experimental-feature Embedded -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

public enum MyError : Error, Equatable {
  case a
  case b
  case c(Int)
}

extension Int : Error {}

public func throwing(which: Int) throws(MyError) -> Int {
  if which == 0 {
    throw MyError.a
  } else if which == 1 {
    throw MyError.b
  } else if which == 2 {
    throw MyError.c(42)
  }

  return 123
}

public func throwing2() throws(Int) {
  throw 42
}

public func catching() {
  do {
    try throwing(which: 0)
  } catch let e as MyError {
    print(e == .a ? "OK1" : "???")
  }

  do {
    try throwing(which: 1)
  } catch let e as MyError where e == .b {
    print(e == .b ? "OK2" : "???")
  } catch {
    print("???")
  }

  do {
    try throwing(which: 2)
  } catch let e as MyError where e == .b {
    print("???")
  } catch {
    print("OK3")
  }

  do {
    try throwing(which: 2)
  } catch let e as MyError {
    if case .c(let n) = e {
      print(n == 42 ? "OK4" : "???") 
    } else {
      print("???")
    }
  } catch {
    print("???")
  }

  do {
    try throwing(which: 3)
    print("OK5")
  } catch {
    print("???")
  }

  do {
    try throwing2()
  } catch {
    print(error == 42 ? "OK6" : "???")
  }
}

@main
struct Main {
  static func main() {
    catching()
  }
}

// CHECK: OK1
// CHECK: OK2
// CHECK: OK3
// CHECK: OK4
// CHECK: OK5
// CHECK: OK6

// CHECK-NOT: ???

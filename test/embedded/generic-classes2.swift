// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s
// RUN: %target-run-simple-swift(-Osize -enable-experimental-feature Embedded -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu

class B<T> {
}

class D<T>: B<T> {
}

func callee<T>(_ t: T.Type) {
  _ = D<T>()
}

public func test() {
  callee(Int.self)
}

@main
struct Main {
  static func main() {
    test()
    print("OK!")
  }
}

// CHECK: OK!

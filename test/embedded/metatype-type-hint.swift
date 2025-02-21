// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -wmo) | %FileCheck %s
// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -wmo -O) | %FileCheck %s
// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -wmo -Osize) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

public struct AsyncStream2<Element> {
  var x: Int
  var y: Int
}

extension AsyncStream2 {
  public static func makeStream2(of elementType: Element.Type = Element.self) -> AsyncStream2<Element> {
    return AsyncStream2<Element>()
  }

  public init(
    _ elementType: Element.Type = Element.self
  ) {
    fatalError()
  }
}

struct MyStruct<T> {
  static func makeStruct(of t: T.Type = T.self) -> MyStruct<T> {
    var s = MyStruct.init()
    return s
  }
  public init(_ t: T.Type = T.self) {
    print("x")
  }
}

@main
struct Main {
  static func main() {
    _ = MyStruct<String>.makeStruct()
    _ = MyStruct.makeStruct(of: String.self)
    print("OK!")
    // CHECK: OK!
  }
}

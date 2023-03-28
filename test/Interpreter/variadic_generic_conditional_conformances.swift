// RUN: %target-run-simple-swift(-Xfrontend -disable-availability-checking)

// REQUIRES: executable_test

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import StdlibUnittest

var conformances = TestSuite("VariadicGenericConformances")

protocol P {
  static func foobar() -> [String]
}

struct G<each T> {}

extension G: P where repeat each T: P {
  static func foobar() -> [String] {
    var result: [String] = []
    repeat result += (each T).foobar()
    return result
  }
}

extension Int: P {
  static func foobar() -> [String] {
    return ["Int"]
  }
}

extension String: P {
  static func foobar() -> [String] {
    return ["String"]
  }
}

func callFoobar<T: P>(_: T) -> [String] {
  return T.foobar()
}

conformances.test("conditional") {
  expectEqual([], callFoobar(G< >()))
  expectEqual(["Int"], callFoobar(G<Int>()))
  expectEqual(["Int", "String"], callFoobar(G<Int, String>()))
}

runAllTests()
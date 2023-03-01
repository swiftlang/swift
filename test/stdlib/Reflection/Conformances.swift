// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: reflection_runtime
// REQUIRES: OS=macosx || OS=ios || OS=tvos || OS=watchos
// UNSUPPORTED: freestanding

import StdlibUnittest

@_spi(Reflection)
import Reflection

let suite = TestSuite("Conformances")

protocol MyCustomProtocol {}

struct Conformer: MyCustomProtocol {}
struct NotConformer {}
struct GenericConformer<T>: MyCustomProtocol {}
struct GenericConditionalConformer<T> {}

extension GenericConditionalConformer: MyCustomProtocol where T: MyCustomProtocol {}

if #available(SwiftStdlib 5.9, *) {
  suite.test("Basic") {
    let customTypes = _typesThatConform(to: (any MyCustomProtocol).self)

    expectNotNil(customTypes)
    expectEqual(customTypes!.count, 1)
    expectTrue(customTypes![0] == Conformer.self)

    let kpType = _typesThatConform(to: (any _AppendKeyPath).self)

    expectNotNil(kpType)
    expectEqual(kpType!.count, 1)
    expectTrue(kpType![0] == AnyKeyPath.self)
  }
}

runAllTests()

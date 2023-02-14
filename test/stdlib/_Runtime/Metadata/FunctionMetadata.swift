// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: reflection
// UNSUPPORTED: freestanding

import StdlibUnittest
import _Runtime

let suite = TestSuite("FunctionMetadata")

if #available(SwiftStdlib 5.9, *) {
  suite.test("Basic") {
    let int = Metadata(Int.self)
    let string = Metadata(String.self)
    let bool = Metadata(Bool.self)

//===----------------------------------------------------------------------===//
// General Queries
//===----------------------------------------------------------------------===//

    let fn0 = Metadata(((Int, String) -> Bool).self).function

    expectFalse(fn0.throws)
    expectTrue(fn0.isEscaping)
    expectFalse(fn0.isDifferential)
    expectFalse(fn0.hasGlobalActor)
    expectFalse(fn0.isAsync)
    expectFalse(fn0.isSendable)
    expectEqual(fn0.convention, .swift)
    expectEqual(fn0.resultMetadata, bool)
    expectTrue(fn0.parameterMetadata.elementsEqual([int, string]))
    expectEqual(fn0.differentiableKind, .nonDifferentiable)

//===----------------------------------------------------------------------===//
// Throws
//===----------------------------------------------------------------------===//

    let fn1 = Metadata((() throws -> ()).self).function

    expectTrue(fn1.throws)

//===----------------------------------------------------------------------===//
// Escaping
//===----------------------------------------------------------------------===//

    // By default, closure arguments to functions are '@nonescaping', however
    // as a type by itself the function is implicitly '@escaping'. I'm unsure
    // of a way to spell a @nonescaping function type in Swift without just
    // using the mangling for it. This mangling is '@nonescaping () -> Int'.

    let fn2 = Metadata(_typeByName("SiyXE")!).function

    expectFalse(fn2.isEscaping)

//===----------------------------------------------------------------------===//
// Differentiable Kind
//===----------------------------------------------------------------------===//

    // Mangling = '@differential(reverse) () -> Int'

    let fn3 = Metadata(_typeByName("SiyYjrc")!).function

    expectTrue(fn3.isDifferential)
    expectEqual(fn3.differentiableKind, .reverse)

    // Mangling = '@differentiable(_forward) (Int) -> Int'

    let fn4 = Metadata(_typeByName("S2iYjfXE")!).function

    expectTrue(fn4.isDifferential)
    expectEqual(fn4.differentiableKind, .forward)

    // Mangling = '@differentiable (Int) -> Int'

    let fn5 = Metadata(_typeByName("S2iYjdXE")!).function

    expectTrue(fn5.isDifferential)
    expectEqual(fn5.differentiableKind, .normal)

    // Mangling = '@differentiable(_linear) (Int) -> Int'

    let fn6 = Metadata(_typeByName("S2iYjlXE")!).function

    expectTrue(fn6.isDifferential)
    expectEqual(fn6.differentiableKind, .linear)

//===----------------------------------------------------------------------===//
// Global Actor
//===----------------------------------------------------------------------===//

    let fn7 = Metadata((@MainActor () -> Int).self).function

    expectTrue(fn7.hasGlobalActor)

//===----------------------------------------------------------------------===//
// Async
//===----------------------------------------------------------------------===//

    let fn8 = Metadata((() async -> Int).self).function

    expectTrue(fn8.isAsync)

//===----------------------------------------------------------------------===//
// Sendable
//===----------------------------------------------------------------------===//

    let fn9 = Metadata((@Sendable () -> Int).self).function

    expectTrue(fn9.isSendable)

//===----------------------------------------------------------------------===//
// Calling Convention
//===----------------------------------------------------------------------===//

    let fn10 = Metadata((@convention(thin) () -> Int).self).function

    expectEqual(fn10.convention, .thin)

    let fn11 = Metadata((@convention(c) () -> Int).self).function

    expectEqual(fn11.convention, .c)

    // Block calling conventions are only supported on platforms with an
    // ObjectiveC interop.
#if canImport(ObjectiveC)
    let fn12 = Metadata((@convention(block) () -> Int).self).function

    expectEqual(fn12.convention, .block)
#endif
  }
}

runAllTests()

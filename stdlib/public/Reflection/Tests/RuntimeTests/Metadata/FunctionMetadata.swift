import XCTest
import _Runtime

extension RuntimeTests {
  func testFunctionMetadata() throws {
    let int = Metadata(Int.self)
    let string = Metadata(String.self)
    let bool = Metadata(Bool.self)
    
//===----------------------------------------------------------------------===//
// General Queries
//===----------------------------------------------------------------------===//
    
    let fn0 = Metadata(((Int, String) -> Bool).self).function
    
    XCTAssertFalse(fn0.throws)
    XCTAssert(fn0.isEscaping)
    XCTAssertFalse(fn0.isDifferential)
    XCTAssertFalse(fn0.hasGlobalActor)
    XCTAssertFalse(fn0.isAsync)
    XCTAssertFalse(fn0.isSendable)
    XCTAssertEqual(fn0.convention, .swift)
    XCTAssertEqual(fn0.resultMetadata, bool)
    XCTAssert(fn0.parameterMetadata.elementsEqual([int, string]))
    XCTAssertEqual(fn0.differentiableKind, .nonDifferentiable)
    
//===----------------------------------------------------------------------===//
// Throws
//===----------------------------------------------------------------------===//
    
    let fn1 = Metadata((() throws -> ()).self).function
    
    XCTAssert(fn1.throws)
    
//===----------------------------------------------------------------------===//
// Escaping
//===----------------------------------------------------------------------===//
    
    // By default, closure arguments to functions are '@nonescaping', however
    // as a type by itself the function is implicitly '@escaping'. I'm unsure
    // of a way to spell a @nonescaping function type in Swift without just
    // using the mangling for it. This mangling is '@nonescaping () -> Int'.
    
    let fn2 = Metadata(_typeByName("SiyXE")!).function
    
    XCTAssertFalse(fn2.isEscaping)
    
//===----------------------------------------------------------------------===//
// Differentiable Kind
//===----------------------------------------------------------------------===//
    
    // Mangling = '@differential(reverse) () -> Int'
    
    let fn3 = Metadata(_typeByName("SiyYjrc")!).function
    
    XCTAssert(fn3.isDifferential)
    XCTAssertEqual(fn3.differentiableKind, .reverse)
    
    // Mangling = '@differentiable(_forward) (Int) -> Int'
    
    let fn4 = Metadata(_typeByName("S2iYjfXE")!).function
    
    XCTAssert(fn4.isDifferential)
    XCTAssertEqual(fn4.differentiableKind, .forward)
    
    // Mangling = '@differentiable (Int) -> Int'
    
    let fn5 = Metadata(_typeByName("S2iYjdXE")!).function
    
    XCTAssert(fn5.isDifferential)
    XCTAssertEqual(fn5.differentiableKind, .normal)
    
    // Mangling = '@differentiable(_linear) (Int) -> Int'
    
    let fn6 = Metadata(_typeByName("S2iYjlXE")!).function
    
    XCTAssert(fn6.isDifferential)
    XCTAssertEqual(fn6.differentiableKind, .linear)
    
//===----------------------------------------------------------------------===//
// Global Actor
//===----------------------------------------------------------------------===//
    
    let fn7 = Metadata((@MainActor () -> Int).self).function
    
    XCTAssert(fn7.hasGlobalActor)
    
//===----------------------------------------------------------------------===//
// Async
//===----------------------------------------------------------------------===//
    
    let fn8 = Metadata((() async -> Int).self).function
    
    XCTAssert(fn8.isAsync)
    
//===----------------------------------------------------------------------===//
// Sendable
//===----------------------------------------------------------------------===//
    
    let fn9 = Metadata((@Sendable () -> Int).self).function
    
    XCTAssert(fn9.isSendable)
    
//===----------------------------------------------------------------------===//
// Calling Convention
//===----------------------------------------------------------------------===//
    
    let fn10 = Metadata((@convention(thin) () -> Int).self).function
    
    XCTAssertEqual(fn10.convention, .thin)
    
    let fn11 = Metadata((@convention(c) () -> Int).self).function
    
    XCTAssertEqual(fn11.convention, .c)
    
    let fn12 = Metadata((@convention(block) () -> Int).self).function
    
    XCTAssertEqual(fn12.convention, .block)
  }
}

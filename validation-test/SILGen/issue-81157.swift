// RUN: %target-swift-emit-silgen %s -verify

// Issue #81157.
//
// Check that SILGen does not crash when mapping between a single value and a
// pack expansion in a reabstraction thunk.
func testParameterPack<each U>(_ arg: repeat each U, function: @escaping (repeat each U) -> Void) { }

func notWorkingFunction(arg: (() -> Void)?) -> Void { }

testParameterPack(nil, function: notWorkingFunction)

// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop

//
//  These tests should work *without* Foundation being present
//

// Unfortunately, StdlibUnittest brings in Foundation somehow, so we
// can't use it here.

// import StdlibUnittest

//===--- Verify that Foundation isn't loaded ------------------------------===//
struct No {}
struct Yes {}
func isRandomAccessIndex<T : ForwardIndexType>(_: T) -> No { return No() }
func isRandomAccessIndex<T : RandomAccessIndexType>(_: T) -> Yes { return Yes() }
let no = isRandomAccessIndex("".utf16.startIndex)
let _: No = no

//===--- Tests ------------------------------------------------------------===//

import ObjectiveC

func expectIsHashable<T : Hashable>(inout value: T) {}
func expectIsCVarArgType<T : CVarArgType>(inout value: T) {}

var anNSObject = NSObject()
expectIsHashable(&anNSObject)
expectIsCVarArgType(&anNSObject)


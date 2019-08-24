//===--- NoFoundation.swift - Tests that run without Foundation loaded ----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  These tests are supposed to work *without* Foundation being present
//
//===----------------------------------------------------------------------===//
// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop

// Unfortunately, StdlibUnittest brings in Foundation somehow, so we
// can't use it here.

// import StdlibUnittest

//===--- Verify that Foundation isn't loaded ------------------------------===//
struct No {}
struct Yes {}
func isRandomAccessCollection<T : Collection>(_: T) -> No { return No() }
func isRandomAccessCollection<T : RandomAccessCollection>(_: T) -> Yes { return Yes() }
let no = isRandomAccessCollection("".utf16)
_ = no as No

//===--- Tests ------------------------------------------------------------===//

import Dispatch
if #available(OSX 10.10, iOS 8.0, *) {
	print(DispatchQueue.global(qos: .default))
}

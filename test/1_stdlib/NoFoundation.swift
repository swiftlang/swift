//===--- NoFoundation.swift - Tests that run without Foundation loaded ----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  These tests are supposed to work *without* Foundation being present
//
//===----------------------------------------------------------------------===//
// RUN: %target-run-simple-swift

// REQUIRES: objc_interop

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

import Dispatch
println(dispatch_get_global_queue(0,0))


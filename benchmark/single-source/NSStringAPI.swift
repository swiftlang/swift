//===--- NSStrignAPI.swift ----------------------------------------------------===//
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

import TestsUtils
import Foundation

@inline(never)
public func run_String_dataUsingEncoding(_ N: Int) {
	let string = String(repeating: "x", count: 4096)
	for _ in 1...N*100 {
		_ = string.data(using: .utf8)
	}
}


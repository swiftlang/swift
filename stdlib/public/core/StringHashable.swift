//===----------------------------------------------------------------------===//
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

import SwiftShims

#if _runtime(_ObjC)
@_silgen_name("swift_stdlib_NSStringHashValue")
func _stdlib_NSStringHashValue(_ str: AnyObject, _ isASCII: Bool) -> Int

@_silgen_name("swift_stdlib_NSStringHashValuePointer")
func _stdlib_NSStringHashValuePointer(_ str: OpaquePointer, _ isASCII: Bool) -> Int
#endif

// Michael NOTE: This file and StringComparable.swift are now much simpler,
// needingly only the few lines in NewString.swift.gyb

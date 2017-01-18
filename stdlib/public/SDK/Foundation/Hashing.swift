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

import CoreFoundation

@_silgen_name("__CFHashInt")
internal func __CFHashInt(_ i: Int) -> CFHashCode

@_silgen_name("__CFHashDouble")
internal func __CFHashDouble(_ d: Double) -> CFHashCode

@_silgen_name("CFHashBytes")
internal func CFHashBytes(_ bytes: UnsafeMutablePointer<UInt8>?, _ length: Int) -> CFHashCode

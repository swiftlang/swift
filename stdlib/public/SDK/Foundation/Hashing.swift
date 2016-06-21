//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import CoreFoundation

@_silgen_name("__CFHashInt")
internal func __CFHashInt(_ i: Int) -> CFHashCode

@_silgen_name("__CFHashDouble")
internal func __CFHashDouble(_ d: Double) -> CFHashCode

@_silgen_name("CFHashBytes")
internal func CFHashBytes(_ bytes: UnsafeMutablePointer<UInt8>, _ length: Int) -> CFHashCode

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

class DummyClass {}

// Used by the weak.mm runtime tests. All that matters is that the returned
// object is an instance of a pure Swift class.
@_silgen_name("make_swift_object")
public func make_swift_object() -> AnyObject {
  return DummyClass()
}

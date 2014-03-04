//===----------------------------------------------------------------------===//
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

// FIXME -- add [rootClass] attribute
// FIXME -- have non-rootClass classes default inherit from this via a typealias
// SEE ALSO -- <rdar://problem/12939349> ER: class attribute: "rootClass"

@asmname="swift_getClassName"
func _className(obj: Object.Type) -> CString

class Object : ClassNameable {
  init () { }

  class func className() -> String {
    // FIXME: Demangle.
    return String.fromCString(_className(self))
  }
}

// XXX -- this next line confuses and then crashes the compiler?!?
// typealias DefaultRootClass = Object


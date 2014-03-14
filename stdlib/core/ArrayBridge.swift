//===--- ArrayBridge.swift - Array<T> <=> NSArray bridging ----------------===//
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

import SwiftShims

// Array<T> may be backed by one of these when T can be an @objc class
@objc @class_protocol
protocol CocoaArray {
  func objectAtIndex(index: Int) -> AnyObject
  func getObjects(UnsafePointer<AnyObject>)range(_SwiftNSRange)
  var count: Int {get}
}

class DumDum {
  init() {
    println("making a DumDum")
  }
  deinit {
    println("destroying a DumDum")
  }
}

// This is a toy that will eventually become the base class of the
// heap buffer implementation backing the new Array design.  For now
// it is just enough to be able to do some experiments.
class NSSwiftArray : CocoaArray {
  func objectAtIndex(index: Int) -> AnyObject {
    return DumDum()
  }
  
  func getObjects(
    objects: UnsafePointer<AnyObject>)range(range: _SwiftNSRange) {

    objects.set(objectAtIndex(0))
  }
  
  var count: Int {
    return 1
  }
}

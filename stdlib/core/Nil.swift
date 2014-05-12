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

struct _Nil : Reflectable {

  @conversion func __conversion<T: RawOptionSet>() -> T {
    return .fromMask(.allZeros())
  }

  func getMirror() -> Mirror {
    return _NilMirror()
  }
}

/// A null sentinel value.
var nil : _Nil {
  return _Nil()
}

struct _NilMirror : Mirror {
  var value: Any { return nil }

  var valueType: Any.Type { return (nil as Any).dynamicType }

  var objectIdentifier: ObjectIdentifier? { return .None }

  var count: Int { return 0 }

  subscript(i: Int) -> (String, Mirror) { fatal("a _NilMirror has no children") }

  var summary: String { return "nil" }

  var quickLookObject: QuickLookObject? { return .None }

  var disposition: MirrorDisposition { return .Aggregate }
}



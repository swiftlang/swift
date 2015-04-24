//===--- Dump.swift -------------------------------------------------------===//
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

import Swift
import SwiftShims // for putchar

//===----------------------------------------------------------------------===//
// FIXME
//===----------------------------------------------------------------------===//

internal struct _Stdout : OutputStreamType {
  mutating func _lock() {
    _swift_stdlib_flockfile_stdout()
  }

  mutating func _unlock() {
    _swift_stdlib_funlockfile_stdout()
  }

  mutating func write(string: String) {
    // FIXME: buffering?
    // It is important that we use stdio routines in order to correctly
    // interoperate with stdio buffering.
    for c in string.utf8 {
      putchar(Int32(c))
    }
  }
}

/// Dump an object's contents using its mirror to the specified output stream.
public func dump<T, TargetStream : OutputStreamType>(
    x: T, inout targetStream: TargetStream,
    name: String? = nil, indent: Int = 0,
    maxDepth: Int = .max, maxItems: Int = .max
) -> T {
  var maxItemCounter = maxItems
  _dump(x,
        Mirror(reflecting: x),
        name,
        indent,
        maxDepth,
        &maxItemCounter,
        &targetStream)
  return x
}

/// Dump an object's contents using its mirror to standard output.
public func dump<T>(x: T, name: String? = nil, indent: Int = 0,
             maxDepth: Int = .max, maxItems: Int = .max) -> T {
  var stdoutStream = _Stdout()
  var maxItemCounter = maxItems
  _dump(x,
        Mirror(reflecting: x),
        name,
        indent,
        maxDepth,
        &maxItemCounter,
        &stdoutStream)
  return x
}

/// Dump an object's contents using a mirror. User code should use dump().
func _dump<TargetStream : OutputStreamType>(
    x_opt: Any?, mirror: Mirror, name: String?, indent: Int, maxDepth: Int,
    inout maxItemCounter: Int,
    inout targetStream: TargetStream
) {
  if maxItemCounter <= 0 { return }
  --maxItemCounter

  for _ in 0..<indent { print(" ", &targetStream) }

  let count = Int(distance(mirror.children.startIndex, mirror.children.endIndex))
  let bullet = count == 0    ? "-"
             : maxDepth <= 0 ? "▹" : "▿"
  print("\(bullet) ", &targetStream)

  if let nam? = name {
    print("\(nam): ", &targetStream)
  }
  if let x? = x_opt {
    print(String(x), &targetStream)
  }

  println("", &targetStream)

  if maxDepth <= 0 { return }
	
	var i = 0
  
  if let superclass_mirror? = mirror.superclassMirror() {
    _dump(nil,
          superclass_mirror,
          "super",
          indent + 2,
          maxDepth - 1,
          &maxItemCounter,
          &targetStream)
  }

  for (name_opt,child) in mirror.children {
		var name: String
		if name_opt == nil { name = "\(i)" }
		else { name = name_opt! }
    if maxItemCounter <= 0 {
      for _ in 0..<(indent+4) { print(" ", &targetStream) }
      let remainder = count - i
      print("(\(remainder)", &targetStream)
      if i > 0 { print(" more", &targetStream) }
      if remainder == 1 {
        println(" child)", &targetStream)
      } else {
        println(" children)", &targetStream)
      }
      return
    }

    _dump(child,
          Mirror(reflecting: child),
          name, 
          indent + 2,
          maxDepth - 1,
          &maxItemCounter,
          &targetStream)
		i++
  }
}
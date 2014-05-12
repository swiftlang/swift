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

struct _Process {
  // Use lazy initialization of static properties to safely initialize the
  // public 'arguments' property on first use.
  static var _arguments: String[] = {
    // FIXME: Use a by-index-initializing constructor of Array here when we
    // have that, so we don't need this awkward closure initialization.
    var _args = new String[Int(C_ARGC)]
    for i in 0...Int(C_ARGC) {
      _args[i] = String.fromCString(C_ARGV[i])
    }
    return _args
  }()

  var arguments : String[] {
    return _Process._arguments
  }
}

var Process : _Process = _Process()

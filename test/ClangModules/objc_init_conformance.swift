// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse %s -verify

// REQUIRES: objc_interop

// Note: this is in a separate file because -verify doesn't complain
// about diagnostics from other files.
import Foundation
import AppKit

// Okay to use an Objective-C-defined initializer to satisfy an
// initializer requirement in a protocol.
protocol URLInitializable {
  init?(URL: String!)
}

extension NSDocument : URLInitializable { }

// Okay to satisfy an 'init' requirement with an 'init!'.
protocol IntInitializable {
  init(int value: Int)
}

extension NSTableViewController : IntInitializable {
}

func testInitWithIntIUO() {
  let tvc = NSTableViewController(int: 5)
  if tvc == nil { }
}



// RUN: %target-swift-frontend -emit-sil -O %s

// REQUIRES: objc_interop

// https://github.com/apple/swift/issues/58337

import Foundation

@objc protocol OptionalVar {
    @objc optional var name: String { get }
}

extension NSObject: OptionalVar { }

do {
  let objects = [NSObject()] as Array<OptionalVar>

  let names = objects.compactMap(\.name)
  print("Names: \(names)")
}

// RUN: %target-run-simple-swift | FileCheck %s

import Foundation

protocol Hashish {
  var hash: Int { get }
}

extension NSObject: Hashish {}

func getHash<T: Hashish>(x: T) -> Int { return x.hash }

let u = NSURL(withString: "http://www.example.com")

// CHECK: true
println(u.hash == getHash(u))

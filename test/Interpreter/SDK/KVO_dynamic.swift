// RUN: rm -rf %t  &&  mkdir -p %t
// RUN: %target-build-swift -Xfrontend -enable-dynamic %s -o %t/a.out
// RUN: %target-run %t/a.out
// XFAIL: *

import Foundation

var kvoContext = 0

class Foo: NSObject {
 let foo = 0
}

let foo = Foo()
foo.addObserver(foo, forKeyPath: "foo", options: nil, context: &kvoContext)
let bar = foo.foo

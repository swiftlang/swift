// RUN: %target-swift-frontend -typecheck -verify %s

import Foundation

class X<T> : NSObject {
    init(x: ()) {
        super.init()
    }
}

class Y : X<Int> {
    init(y: ()) {
        super.init(x: ())
    }
}

extension Y {
    @objc func f() {} // expected-error{{@objc is not supported within extensions of generic classes, or classes that inherit from generic classes}}
}

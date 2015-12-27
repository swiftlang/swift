// RUN: not --crash %target-swift-frontend %s -parse
// REQUIRES: objc_interop

// Distributed under the terms of the MIT license
// Test case found by https://github.com/allu22 (Alvar Hansen)

class A {
    func a() {
        A().d { [weak self] (c) -> Void in
            self?.b(c)
        }
    }
    func d(e: ((AnyObject)->Void)) {
    }
    func b(f: AnyObject, g: AnyObject) {
    }
}

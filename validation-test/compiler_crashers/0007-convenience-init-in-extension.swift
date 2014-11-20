// RUN: %target-swift-frontend %s -emit-ir

// Test case submitted to project by https://github.com/0xc010d (Ievgen Solodovnykov)

class A {
    init() {
    }
}

extension A {
    convenience init(i: Int) {
        self.init()
    }

    convenience init(s: String) {
        self.init(i: 1)
    }
}

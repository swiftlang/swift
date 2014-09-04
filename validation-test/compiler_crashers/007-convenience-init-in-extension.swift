// RUN: %swift %s -parse -verify
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
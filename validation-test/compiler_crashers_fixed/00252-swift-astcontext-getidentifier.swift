// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
impor {
        return ""
    }
    class func b() {
        struct c {
            static let d: String = {
                return self.a()
  }
}
func b(c) -> <d>(() -> d) {
}
struct d<f : P {
    func f<T>() -> T -> T {
        return { x in x }
    }
}
protocol P {
    func f<T>()(T) -> T
}

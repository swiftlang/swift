// RUN: %target-swift-frontend -primary-file %s -emit-ir -o -

// https://github.com/apple/swift/issues/49830

struct S<T> {}

extension S where T == Int {
    typealias Callback = (Bool) -> Void

    func process(callback: Callback) {   
    }
}

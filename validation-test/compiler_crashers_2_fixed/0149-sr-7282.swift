// RUN: %target-swift-frontend -primary-file %s -emit-ir -o -

// SR-7282
struct S<T> {}

extension S where T == Int {
    typealias Callback = (Bool) -> Void

    func process(callback: Callback) {   
    }
}

// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values %s
// RUN: %target-swift-emit-silgen -verify %s

extension Unmanaged {
    func something1(with x: consuming Unmanaged<Instance>?) -> UnsafeMutableRawPointer? {
        x?.toOpaque()
    }

    func something2(with x: consuming Unmanaged<Instance>?) -> UnsafeMutableRawPointer? {
        let y = x
        return y?.toOpaque()
    }
    func something3(with x: __owned Unmanaged<Instance>?) -> UnsafeMutableRawPointer? {
        x?.toOpaque()
    }
}

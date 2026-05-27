// RUN: %target-swift-emit-sil -O -wmo -sanitize=thread -cxx-interoperability-mode=default -swift-version 6 -I %S/Inputs %s | %FileCheck %s
// REQUIRES: OS=macosx

import TsanCxxWitnessCopy

protocol VecProto {
    associatedtype Element
    func size() -> CInt
    func __atUnsafe(_ i: CInt) -> UnsafePointer<Element>?
}

extension RefVec: VecProto {
    typealias Element = Ref
}

struct VecIter<V: VecProto> {
    var vec: V
    var pos: CInt = 0

    @inline(never)
    mutating func next() -> UnsafePointer<V.Element>? {
        guard pos < vec.size() else { return nil }
        let item = vec.__atUnsafe(pos)
        pos += 1
        return item
    }
}

@inline(never)
func iterate(vec: RefVec) -> UnsafePointer<Ref>? {
    var iter = VecIter(vec: vec)
    return iter.next()
}

public func testEntry() -> UnsafePointer<Ref>? {
    let vec = RefVec()
    return iterate(vec: vec)
}

// The optimized SIL for `next()` should pass `self.vec` directly to the
// protocol witness — no temporary copy.
//
// CHECK-LABEL: sil {{.*}}4next{{.*}}
// CHECK-NOT: copy_addr
// CHECK-LABEL: } // end sil function

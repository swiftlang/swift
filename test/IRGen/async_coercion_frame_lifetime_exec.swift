// RUN: %target-run-simple-swift(-O) | %FileCheck %s
// RUN: %target-run-simple-swift(-Onone) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime

// End-to-end regression test for an IRGen miscompile at -O.
//
// A call-argument coercion temporary (irgen::allocateForCoercion) was created
// with an llvm.lifetime marker sized in *bits* rather than *bytes* (8x too
// large). Once that temporary was coalesced into the async coroutine frame by
// CoroSplit, the oversized lifetime.end covered adjacent, still-live frame slots
// -- here the spilled `self` -- and DSE then deleted those live stores. `self`
// (and hence `a`/`b`) read back as zero at runtime. The enum payload must be
// large enough (>= 8 bytes) to reach across the frame to the `self` slots.

enum Project {
    case projectID((UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8))
}

struct Holder {
    let a = 67_108_864
    let b = 6

    func guardProbe(of project: Project) async {
        guard case .projectID(let id) = project else { return }
        // Before the fix, at -O this printed "self=Holder(a: 0, b: 0) a=0 b=0".
        // CHECK: guard: self=Holder(a: 67108864, b: 6) a=67108864 b=6 id=66
        print("guard: self=\(self) a=\(a) b=\(b) id=\(id.0)")
    }
}

await Holder().guardProbe(of: .projectID((66, 2, 3, 4, 5, 6, 7, 8, 9)))

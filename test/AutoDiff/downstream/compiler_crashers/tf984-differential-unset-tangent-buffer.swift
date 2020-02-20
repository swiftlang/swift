// RUN: not --crash %target-swift-emit-sil -enable-experimental-forward-mode-differentiation %s -verify
// REQUIRES: asserts

// TF-984: Differential generation crash due to unset tangent buffer.

struct Mut: Differentiable {}
extension Mut {
  mutating func mutatingMethodWrtMultipleResults(_ x: Mut) -> Mut {
    return x
  }
}

@differentiable(wrt: x)
func activeInoutArgMutatingMethodVar(_ nonactive: inout Mut, _ x: Mut) {
  nonactive.mutatingMethodWrtMultipleResults(x)
}

// [AD] Original bb0: To differentiate or not to differentiate?
// [ ]   debug_value_addr %0 : $*Mut, var, name "nonactive", argno 1 // id: %2
// [ ]   debug_value %1 : $Mut, let, name "x", argno 2   // id: %3
// [∂]   %4 = alloc_stack $Mut, var, name "result"       // users: %18, %6, %8, %12, %15
// [ ]   %5 = begin_access [read] [static] %0 : $*Mut    // users: %7, %6
// [∂]   copy_addr %5 to [initialization] %4 : $*Mut     // id: %6
// ...
// [AD] JVPEmitter visited:
// [ORIG]  copy_addr %5 to [initialization] %4 : $*Mut     // id: %6
// Assertion failed: (!insertion.second && "tangent buffer should already exist"), function getTangentBuffer, file swift/lib/SILOptimizer/Mandatory/Differentiation.cpp, line 4528.

// `copy_addr %5 to [initialization] %4` is visited but `%5 = begin_access` is
// not. `%5` does not have a set tangent buffer.

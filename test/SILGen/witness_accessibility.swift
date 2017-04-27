// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -emit-silgen %s | %FileCheck %s

public protocol P {
  func publicRequirement()
}

protocol Q : P {
  func internalRequirement()
}

fileprivate protocol R : Q {
  func privateRequirement()
}

extension R {
  public func publicRequirement() {}
  func internalRequirement() {}
  func privateRequirement() {}
}

public struct S : R {}

// CHECK-LABEL: sil private @_T021witness_accessibility1R{{.*}}AE18privateRequirementyyF
// CHECK-LABEL: sil private @_T021witness_accessibility1R{{.*}}E17publicRequirementyyF
// CHECK-LABEL: sil private @_T021witness_accessibility1R{{.*}}E19internalRequirementyyF

// CHECK-LABEL: sil private [transparent] [thunk] @_T021witness_accessibility1SVAA1R{{.*}}dELLP18privateRequirementyyFTW
// CHECK-LABEL: sil private [transparent] [thunk] @_T021witness_accessibility1SVAA1QA2aDP19internalRequirementyyFTW

// FIXME: This is public because of an explicit workaround for
// the default implementation of publicRequirement() having the
// wrong linkage.
//
// It should be 'shared [fragile]', but it's not clear that this
// this makes sense either, because 'R' is private.

// Perhaps the conformance S : R should be rejected, because
// publicRequirement() is not sufficiently visible; furthermore,
// the use of the 'public' keyword inside an extension of 'R'
// should generate a warning, since it has no effect.

// CHECK-LABEL: sil [transparent] [thunk] @_T021witness_accessibility1SVAA1PA2aDP17publicRequirementyyFTW

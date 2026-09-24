// RUN: %target-swift-frontend -emit-sil -O -module-name src %s | %FileCheck %s

@inline(never)
public func invoke(_ x: Double, _ body: (Double) -> Double) -> Double {
  body(x)
}

// Run SILCombine immediately before ClosureSpecialization to expose a direct
// call to `invoke` before the new specialization of `forward` is visited.

// CHECK:       // specialized forward<A>(_:_:)
// CHECK-LABEL: sil shared [noinline] @$s3src7forwardyxx_xx_S2dXEtXEtlFSd_Tg5018$sS3dIgyd_SdIgygd_de1_F46Iegngr_TR018$s3src6invokeyS2d_B4XEtFTf3nnnpf_nTf1nc_n : $@convention(thin) (Double) -> Double {
// CHECK:       bb0([[X:%[0-9]+]] : $Double):
// CHECK-NOT:     thin_to_thick_function
// CHECK-NOT:     partial_apply
// CHECK:         // function_ref specialized invoke(_:_:)
// CHECK-NEXT:    [[INVOKE:%[0-9]+]] = function_ref @$s3src6invokeyS2d_S2dXEtF40$s3src7forwardyxx_xx_S2dXEtXEtlFS2dXEfU_Tf1nc_n : $@convention(thin) (Double) -> Double
// CHECK-NEXT:    [[RESULT:%[0-9]+]] = apply [[INVOKE]]([[X]]) : $@convention(thin) (Double) -> Double
// CHECK-NEXT:    return [[RESULT]]
// CHECK-LABEL: } // end sil function '$s3src7forwardyxx_xx_S2dXEtXEtlFSd_Tg5018$sS3dIgyd_SdIgygd_de1_F46Iegngr_TR018$s3src6invokeyS2d_B4XEtFTf3nnnpf_nTf1nc_n'

@inline(never)
public func forward<T>(_ x: T, _ body: (T, (Double) -> Double) -> T) -> T {
  body(x, { $0 * $0 })
}

public func test(_ x: Double) -> Double {
  forward(x, invoke)
}

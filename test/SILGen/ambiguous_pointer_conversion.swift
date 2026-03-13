// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

// rdar://146780049

class Image {}

class MyV {
    var image: Image = Image()
}

func takesPtr(_: UnsafePointer<Image>?, _: UnsafePointer<Image>?, _: UnsafePointer<Image>?) {
}

func test(buffers: [MyV]) {
    withUnsafePointer(to: buffers[0].image) { ptrA in
        withUnsafePointer(to: buffers[1].image) { ptrB in
            withUnsafePointer(to: buffers[2].image) { ptrC in
                _ = takesPtr(ptrA, ptrB, ptrC)
            }
        }
    }
}

// The other valid conversion here that should not be considered because it has a worse
// score is pointer-to-pointer instead of value-to-optional. The generated SIL looks
// quite different in that case, involving calls to intrinsics. Make sure we pick the
// value-to-optional conversion, because it generates much simpler SIL:

// CHECK-LABEL: sil private [ossa] @$s28ambiguous_pointer_conversion4test7buffersySayAA3MyVCG_tFySPyAA5ImageCGXEfU_yAIXEfU_yAIXEfU_ : $@convention(thin) @substituted <τ_0_0, τ_0_1, τ_0_2> (UnsafePointer<τ_0_0>, UnsafePointer<Image>, UnsafePointer<Image>) -> (@out τ_0_2, @error_indirect τ_0_1) for <Image, Never, ()> {
// CHECK: bb0(%0 : $*(), %1 : $*Never, %2 : $UnsafePointer<Image>, %3 : @closureCapture $UnsafePointer<Image>, %4 : @closureCapture $UnsafePointer<Image>):
// CHECK: [[X:%.*]] = enum $Optional<UnsafePointer<Image>>, #Optional.some!enumelt, %3
// CHECK: [[Y:%.*]] = enum $Optional<UnsafePointer<Image>>, #Optional.some!enumelt, %4
// CHECK: [[Z:%.*]] = enum $Optional<UnsafePointer<Image>>, #Optional.some!enumelt, %2
// CHECK: [[FN:%.*]] = function_ref @$s28ambiguous_pointer_conversion8takesPtryySPyAA5ImageCGSg_A2FtF : $@convention(thin) (Optional<UnsafePointer<Image>>, Optional<UnsafePointer<Image>>, Optional<UnsafePointer<Image>>) -> ()
// CHECK: apply %11([[X]], [[Y]], [[Z]]) : $@convention(thin) (Optional<UnsafePointer<Image>>, Optional<UnsafePointer<Image>>, Optional<UnsafePointer<Image>>) -> ()
// CHECK: return


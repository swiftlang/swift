// RUN: %target-swift-frontend -import-objc-header %S/Inputs/objc_init_blocks.h %s -emit-sil -sil-verify-all

// REQUIRES: objc_interop

// rdar://126142109: import an __unsafe_unretained block as zero-initialized.
//
// Make sure that the SIL ownership verifier passes.
// UnsafeUnretainedBlockClass.init()
// CHECK-LABEL: sil hidden @$s16objc_init_blocks26UnsafeUnretainedBlockClassCACycfc : $@convention(method) (@owned UnsafeUnretainedBlockClass) -> @owned UnsafeUnretainedBlockClass {
// CHECK: [[ZI:%.*]] = builtin "zeroInitializer"() : $objc_bool_block
// CHECK:   store [[ZI]] to %{{.*}} : $*objc_bool_block
// CHECK-LABEL: } // end sil function '$s16objc_init_blocks26UnsafeUnretainedBlockClassCACycfc'
open class UnsafeUnretainedBlockClass {
  public internal(set) var sc: objc_bool_block
  init() {
    self.sc = objc_bool_block()
  }
}

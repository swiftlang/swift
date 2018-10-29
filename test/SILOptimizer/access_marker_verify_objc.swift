// RUN: %target-swift-frontend -enforce-exclusivity=checked -enable-sil-ownership -import-objc-header %S/Inputs/access_marker_verify_objc.h -Onone -emit-silgen -swift-version 4 -parse-as-library %s | %FileCheck %s
// RUN: %target-swift-frontend -enable-verify-exclusivity -enforce-exclusivity=checked -enable-sil-ownership -import-objc-header %S/Inputs/access_marker_verify_objc.h -Onone -emit-sil -swift-version 4 -parse-as-library %s
// REQUIRES: asserts
// REQUIRES: OS=macosx

// Test the combination of SILGen + DiagnoseStaticExclusivity with verification.
//
// This augments access_marker_verify with tests that require ObjC or
// CoreFoundation.
import Foundation

// --- initializer `let` of CFString.
// The verifier should ignore this.

// CHECK_LABEL: sil private @globalinit{{.*}} : $@convention(c) () -> () {
// CHECK: bb0:
// CHECK:   alloc_global @$s25access_marker_verify_objc12testCFStringC8cfStringSo0F3RefavpZ
// CHECK:   [[GA:%.*]] = global_addr @$s25access_marker_verify_objc12testCFStringC8cfStringSo0F3RefavpZ : $*CFString
// CHECK-NOT: begin_access
// CHECK:   store %{{.*}} to [init] [[GA]] : $*CFString
// CHECK:   return %{{.*}} : $()                               
// CHECK-LABEL: } // end sil function 'globalinit{{.*}}'
class testCFString {
  public static let cfString: CFString = "" as CFString
}

// --- objC method.
// The verifier should be able to handle a thunk of a block and ignore
// an incoming block argument.
@objc protocol HasBlock {
  func block(_: (Int) -> Int)
}

class HasBlockImpl: HasBlock {
  @objc func block(_: (Int) -> Int) {}
}
// CHECK-LABEL: sil hidden [thunk] @$s25access_marker_verify_objc12HasBlockImplC5blockyyS2iXEFTo : $@convention(objc_method) (@convention(block) @noescape (Int) -> Int, HasBlockImpl) -> () {
// CHECK: bb0(%0 : @unowned $@convention(block) @noescape (Int) -> Int, %1 : @unowned $HasBlockImpl):
// CHECK:   [[CP:%.*]] = copy_block %0 : $@convention(block) @noescape (Int) -> Int
            // function_ref thunk for @callee_unowned @convention(block) (@unowned Int) -> (@unowned Int)
// CHECK:   [[THUNK:%.*]] = function_ref @$sS2iIyByd_S2iIegyd_TR : $@convention(thin) (Int, @guaranteed @convention(block) @noescape (Int) -> Int) -> Int
// CHECK:   [[PA:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[CP]]) : $@convention(thin) (Int, @guaranteed @convention(block) @noescape (Int) -> Int) -> Int
// CHECK:   [[CVT:%.*]] = convert_escape_to_noescape [not_guaranteed] [[PA]] : $@callee_guaranteed (Int) -> Int to $@noescape @callee_guaranteed (Int) -> Int
// CHECK:   [[F:%.*]] = function_ref @$s25access_marker_verify_objc12HasBlockImplC5blockyyS2iXEF : $@convention(method) (@noescape @callee_guaranteed (Int) -> Int, @guaranteed HasBlockImpl) -> ()
// CHECK:   %{{.*}} = apply [[F]]([[CVT]], %{{.*}}) : $@convention(method) (@noescape @callee_guaranteed (Int) -> Int, @guaranteed HasBlockImpl) -> ()
// CHECK:   return %{{.*}} : $()                                
// CHECK-LABEL: } // end sil function '$s25access_marker_verify_objc12HasBlockImplC5blockyyS2iXEFTo'

// thunk for @callee_unowned @convention(block) (@unowned Int) -> (@unowned Int)
// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$sS2iIyByd_S2iIegyd_TR : $@convention(thin) (Int, @guaranteed @convention(block) @noescape (Int) -> Int) -> Int {
// CHECK: bb0(%0 : @trivial $Int, %1 : @guaranteed $@convention(block) @noescape (Int) -> Int):
// CHECK:   %{{.*}} = apply %1(%0) : $@convention(block) @noescape (Int) -> Int
// CHECK:  return %{{.*}} : $Int                               
// CHECK-LABEL: } // end sil function '$sS2iIyByd_S2iIegyd_TR'

// --- C global.
// The verifier should ignore this access.
// CHECK-LABEL: sil hidden @$s25access_marker_verify_objc14GlobalPropertyC14globalCFStringSo0H3RefavgZ : $@convention(method) (@thick GlobalProperty.Type) -> @owned CFString {
// CHECK: bb0(%0 : @trivial $@thick GlobalProperty.Type):
// CHECK:   [[GA:%.*]] = global_addr @constCGlobal : $*Optional<CFString>
// CHECK:   [[STR:%.*]] = load [copy] [[GA]] : $*Optional<CFString>            
// CHECK: switch_enum [[STR]] : $Optional<CFString>, case #Optional.some!enumelt.1: [[SOMEBB:bb.*]], case #Optional.none!enumelt: bb{{.*}}
// CHECK:   [[SOMEBB]]([[R:%.*]] : @owned $CFString):
// CHECK:   return [[R]] : $CFString
// CHECK_LABEL: } // end sil function '$s25access_marker_verify_objc14GlobalPropertyC14globalCFStringSo0H3RefavgZ'
class GlobalProperty {
  public class var globalCFString: CFString { return constCGlobal }
}

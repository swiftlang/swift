// RUN: %empty-directory(%t)

// RUN: %target-swift-emit-silgen %s -module-name cdecl \
// RUN:   -enable-experimental-feature CDecl > %t/out.sil
// RUN: %FileCheck %s -input-file %t/out.sil

// REQUIRES: swift_feature_CDecl

// CHECK-LABEL: sil hidden [ossa] @pear : $@convention(c) (@convention(c) (Int) -> Int) -> () {
@c(pear)
func apple(_ f: @convention(c) (Int) -> Int) { }

func acceptSwiftFunc(_ f: (Int) -> Int) { }

// CHECK-LABEL: sil hidden [ossa] @$s5cdecl16forceCEntryPoint{{[_0-9a-zA-Z]*}}F
// CHECK: [[GRAPEFRUIT:%[0-9]+]] = function_ref @grapefruit : $@convention(c) (Int) -> Int
// CHECK: [[PEAR:%[0-9]+]] = function_ref @pear : $@convention(c) (@convention(c) (Int) -> Int) -> ()
// CHECK: apply [[PEAR]]([[GRAPEFRUIT]])
func forceCEntryPoint() {
  apple(orange)
}

// CHECK-LABEL: sil hidden [ossa] @grapefruit : $@convention(c) (Int) -> Int {
@c(grapefruit)
func orange(_ x: Int) -> Int {
  return x
}

// CHECK-LABEL: sil hidden [ossa] @$s5cdecl13requiresThunkyyF : $@convention(thin) () -> () {
// CHECK: [[ORANGE:%[0-9]+]] = function_ref @$s5cdecl6orangeyS2iFTO : $@convention(thin) (Int) -> Int
// CHECK: [[THICK_ORANGE:%[0-9]+]] = thin_to_thick_function [[ORANGE]] to $@callee_guaranteed (Int) -> Int
// CHECK: [[NOESCAPE_ORANGE:%[0-9]+]] = convert_escape_to_noescape [not_guaranteed] [[THICK_ORANGE]] to $@noescape @callee_guaranteed (Int) -> Int
// CHECK: [[ACCEPT:%[0-9]+]] = function_ref @$s5cdecl15acceptSwiftFuncyyS2iXEF : $@convention(thin) (@guaranteed @noescape @callee_guaranteed (Int) -> Int) -> ()
// CHECK: apply [[ACCEPT]]([[NOESCAPE_ORANGE]])
func requiresThunk() {
  acceptSwiftFunc(orange)
}

// CHECK-LABEL: sil [serialized] [ossa] @cauliflower : $@convention(c) (Int) -> Int {
// CHECK-NOT: apply
// CHECK: return
@c(cauliflower)
public func broccoli(_ x: Int) -> Int {
  return x
}

// CHECK-LABEL: sil private [ossa] @collard_greens : $@convention(c) (Int) -> Int {
// CHECK-NOT: apply
// CHECK: return
@c(collard_greens)
private func kale(_ x: Int) -> Int {
  return x
}

// CHECK-LABEL: sil private [ossa] @defaultName : $@convention(c) (Int) -> Int {
// CHECK-NOT: apply
// CHECK: return
@c
private func defaultName(_ x: Int) -> Int {
  return x
}

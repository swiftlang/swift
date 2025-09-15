// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name test %s | %FileCheck %s --enable-var-scope

class Retainable {}

struct HasStatic : ~Copyable {
  var x = 1
  var y = Retainable()
  static let b = HasStatic()
}

// coverage for rdar://117082469
// CHECK-LABEL: sil hidden [ossa] @$s4test0A11HasStatic_1yyF : $@convention(thin) () -> () {
// CHECK:         [[ADDRESSOR:%.*]] = function_ref @$s4test9HasStaticV1bACvau : $@convention(thin) () -> Builtin.RawPointer
// CHECK:         [[PTR:%.*]] = apply [[ADDRESSOR]]() : $@convention(thin) () -> Builtin.RawPointer
// CHECK:         [[ADDR:%.*]] = pointer_to_address [[PTR]] : $Builtin.RawPointer to [strict] $*HasStatic
// CHECK:         [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ADDR]] : $*HasStatic
// CHECK:         [[X_ADDR:%.*]] = struct_element_addr [[MARK]] : $*HasStatic, #HasStatic.x
// CHECK:         = load [trivial] [[X_ADDR]] : $*Int
func testHasStatic_1() {
  _ = HasStatic.b.x
}

// CHECK-LABEL: sil hidden [ossa] @$s4test0A11HasStatic_2yyF : $@convention(thin) () -> () {
// CHECK:         [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] {{%.*}} : $*HasStatic
// CHECK:         [[Y_ADDR:%.*]] = struct_element_addr [[MARK]] : $*HasStatic, #HasStatic.y
// CHECK:         = load [copy] [[Y_ADDR]] : $*Retainable
func testHasStatic_2() {
  _ = HasStatic.b.y
}


struct NormalType {}
func expectWrapper(_ a : consuming NormalType) {}

// CHECK-LABEL: sil hidden [ossa] @$s4test13expectWrapperyyAA10NormalTypeVnF : $@convention(thin) (NormalType) -> () {
// CHECK:         bb0({{.*}} : @noImplicitCopy @_eagerMove $NormalType):
// CHECK:           alloc_box ${ var @moveOnly NormalType }, var

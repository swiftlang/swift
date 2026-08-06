// RUN: %target-swift-frontend -emit-silgen-ossa -enable-lifetime-resolution \
// RUN:   -enable-experimental-feature LifetimeDependence %s | %FileCheck %s

// REQUIRES: swift_feature_LifetimeDependence

class Kl {}
enum Enum { case some(Kl); case none }
enum Trivial { case red, green }

func Use(_ k: Kl) {}
func Consume(_ k: __owned Kl) {}

// A consume as the last use needs no copy, and no destroy survives.

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}BorrowThenConsume
// CHECK:      [[X:%.*]] = move_value [lexical] [var_decl]
// CHECK:      apply {{%.*}}([[X]]) : $@convention(thin) (@guaranteed Kl) -> ()
// CHECK:      apply {{%.*}}([[X]]) : $@convention(thin) (@owned Kl) -> ()
// CHECK-NOT:  copy_value
// CHECK-NOT:  destroy_value
// CHECK-LABEL: } // end sil function
func BorrowThenConsume() {
  let x = Kl()
  Use(x)
  Consume(x)
}

// A consume in the interior of the live range takes exactly one copy; the original is
// destroyed after its last use.

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}ConsumeThenBorrow
// CHECK:      [[X:%.*]] = move_value [lexical] [var_decl]
// CHECK:      [[COPY:%.*]] = copy_value [[X]]
// CHECK-NEXT: apply {{%.*}}([[COPY]]) : $@convention(thin) (@owned Kl) -> ()
// CHECK:      apply {{%.*}}([[X]]) : $@convention(thin) (@guaranteed Kl) -> ()
// CHECK-NEXT: destroy_value [[X]]
// CHECK-LABEL: } // end sil function
func ConsumeThenBorrow() {
  let x = Kl()
  Consume(x)
  Use(x)
}

// A `var` is box-backed, so its uses sit inside a `begin_borrow [lexical]` scope. The box's
// destroy must stay below the `end_borrow` that closes that scope.

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}VarReassign
// CHECK:      [[BOX:%.*]] = alloc_box ${ var Kl }
// CHECK-NEXT: [[BORROW:%.*]] = begin_borrow [lexical] [var_decl] [[BOX]]
// CHECK:      end_borrow [[BORROW]]
// CHECK-NEXT: destroy_value [[BOX]]
// CHECK-LABEL: } // end sil function
func VarReassign() {
  var x = Kl()
  Consume(x)
  x = Kl()
  Use(x)
}

// The `@owned` enum is a block argument's source: SILGen borrows it to switch, so the
// payload is guaranteed and its `copy_value` is a real ownership conversion that must
// survive. The enum's own destroy sinks into both arms.

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}SwitchPayload
// CHECK:      bb0([[E:%.*]] : @owned $Enum):
// CHECK:      [[B:%.*]] = begin_borrow [[E]]
// CHECK:      switch_enum [[B]]
// CHECK:      bb1([[PAYLOAD:%.*]] : @guaranteed $Kl):
// CHECK-NEXT: [[COPY:%.*]] = copy_value [[PAYLOAD]]
// CHECK-NEXT: [[K:%.*]] = move_value [lexical] [var_decl] [[COPY]]
// CHECK:      apply {{%.*}}([[K]]) : $@convention(thin) (@owned Kl) -> ()
// CHECK-NEXT: end_borrow [[B]]
// CHECK-NEXT: destroy_value [[E]]
// CHECK-LABEL: } // end sil function
func SwitchPayload(_ e: __owned Enum) {
  switch e {
  case .some(let k): Consume(k)
  case .none: break
  }
}

// An `@owned` parameter is a block argument, which has no defining instruction. It still
// gets resolved: its destroy lands right after its last use.

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}OwnedArgument
// CHECK:      bb0([[K:%.*]] : @owned $Kl):
// CHECK:      apply {{%.*}}([[K]]) : $@convention(thin) (@guaranteed Kl) -> ()
// CHECK-NEXT: destroy_value [[K]]
// CHECK-LABEL: } // end sil function
func OwnedArgument(_ k: __owned Kl) {
  Use(k)
}

// A trivial value has no lifetime to resolve. The synthesized `__derived_enum_equals` is
// full of `load [trivial]`, which must never reach destroy insertion.

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}TrivialCompare
// CHECK-NOT:  destroy_value
// CHECK-LABEL: } // end sil function
func TrivialCompare(_ a: Trivial, _ b: Trivial) -> Bool {
  return a == b
}

// The implicit String-to-C-pointer conversion emits a bare (i.e. [escaping])
// `mark_dependence` on the owner returned by the conversion. The owner's destroy must stay
// below the call that consumes the pointer; hoisting it to the mark_dependence makes the
// callee read freed memory.

func TakesCPointer(_ p: UnsafePointer<CChar>) {}

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}StringToPointer
// CHECK:      [[OWNER:%[0-9]+]] = apply {{.*}}@owned Optional<AnyObject>
// CHECK:      [[DEP:%.*]] = mark_dependence {{%.*}} on [[OWNER]]
// CHECK:      apply {{%.*}}([[DEP]]) : $@convention(thin) (UnsafePointer<Int8>) -> ()
// CHECK-NEXT: destroy_value [[OWNER]]
// CHECK-LABEL: } // end sil function
func StringToPointer(_ s: String) {
  TakesCPointer(s)
}

// A ~Escapable dependent value's uses are enumerable, so they extend the base's liveness. The
// base must outlive the dependent, not the other way around.

struct NE: ~Escapable { let p: UnsafeRawPointer }
class Buf { let p = UnsafeRawPointer(bitPattern: 1)! }

func MakeNE(_ b: borrowing Buf) -> NE { NE(p: b.p) }
func UseNE(_ n: borrowing NE) {}

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}NEDependent
// CHECK:      [[B:%.*]] = move_value [lexical] [var_decl]
// CHECK:      [[DEP:%.*]] = mark_dependence [unresolved] {{%.*}} on [[B]]
// CHECK-NEXT: [[N:%.*]] = move_value [var_decl] [[DEP]]
// CHECK:      apply {{%.*}}([[N]]) : $@convention(thin) (@guaranteed NE) -> ()
// CHECK-NEXT: destroy_value [[N]]
// CHECK-NEXT: destroy_value [[B]]
// CHECK-LABEL: } // end sil function
func NEDependent() {
  let b = Buf()
  let n = MakeNE(b)
  UseNE(n)
}

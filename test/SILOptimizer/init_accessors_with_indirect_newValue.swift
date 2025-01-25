// RUN: %target-swift-frontend -primary-file %s -Onone -Xllvm -sil-print-types -emit-sil \
// RUN:   -enable-library-evolution \
// RUN:   -Xllvm -sil-print-types -Xllvm -sil-print-after=definite-init \
// RUN:   -o /dev/null -module-name init_accessors 2>&1 | %FileCheck %s

public class Test {
  public enum State: Equatable {
  case start
  case failed(any Swift.Error)
  case completed

    public static func == (lhs: Self, rhs: Self) -> Bool { false }
  }

  private var _state: State = .start {
    didSet { }
  }

  public private(set) var state: State = .start {
    @storageRestrictions(initializes: _state)
    init {
      _state = newValue
    }

    get { _state }
    set { _state = newValue }
  }

  // CHECK-LABEL: sil [ossa] @$s14init_accessors4TestCACycfc : $@convention(method) (@owned Test) -> @owned Test
  //
  // CHECK: [[MU:%.*]] = mark_uninitialized [rootself] %0 : $Test
  // CHECK: [[DEFAULT_VALUE_INIT:%.*]] = function_ref @$s14init_accessors4TestC5stateAC5StateOvpfi : $@convention(thin) () -> @out Test.State
  // CHECK-NEXT: [[DEFAULT_VALUE_SLOT:%.*]] = alloc_stack $Test.State
  // CHECK-NEXT: {{.*}} = apply [[DEFAULT_VALUE_INIT]]([[DEFAULT_VALUE_SLOT]]) : $@convention(thin) () -> @out Test.State
  // CHECK-NEXT: [[DEFAULT_VALUE:%.*]] = load [take] [[DEFAULT_VALUE_SLOT]] : $*Test.State
  // CHECK: [[NEW_VALUE:%.*]] = alloc_stack $Test.State
  // CHECK-NEXT: store [[DEFAULT_VALUE]] to [init] [[NEW_VALUE]] : $*Test.State
  // CHECK: assign_or_init [init] #Test.state, self [[MU]] : $Test, value [[NEW_VALUE]] : $*Test.State, init {{.*}} : $@noescape @callee_guaranteed (@in Test.State) -> @out Test.State, set {{.*}} : $@noescape @callee_guaranteed (@in Test.State) -> ()
  //
  // CHECK: [[START_STATE:%.*]] = enum $Test.State, #Test.State.start!enumelt
  // CHECK-NEXT: [[NEW_VALUE:%.*]] = alloc_stack $Test.State
  // CHECK-NEXT: store [[START_STATE]] to [trivial] [[NEW_VALUE]] : $*Test.State
  // CHECK: assign_or_init [set] #Test.state, self [[MU]] : $Test, value [[NEW_VALUE]] : $*Test.State, init {{.*}} : $@noescape @callee_guaranteed (@in Test.State) -> @out Test.State, set {{.*}} : $@noescape @callee_guaranteed (@in Test.State) -> ()
  public init() {
    state = .start
  }
}

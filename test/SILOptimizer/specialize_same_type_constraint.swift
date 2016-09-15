// RUN: %target-swift-frontend  -O -emit-sil -primary-file %s | %FileCheck %s

protocol FirstChild {}

protocol FirstParent {
  associatedtype Child : FirstChild

  var firstChild: Child { get }
}

protocol SecondChild {}

protocol SecondParent {
  associatedtype Child : SecondChild

  var secondChild: Child { get }
}

@_semantics("optimize.sil.never")
func takesFirstChild<T : FirstChild>(t: T) {}

@_semantics("optimize.sil.never")
func takesSecondChild<T : SecondChild>(t: T) {}

@inline(never)
func doStuff<First : FirstParent, Second : SecondParent>(f: First, s: Second)
    where First.Child == Second.Child {
  takesFirstChild(t: f.firstChild)
  takesSecondChild(t: f.firstChild)

  takesFirstChild(t: s.secondChild)
  takesSecondChild(t: s.secondChild)
}

struct ConcreteChild : FirstChild, SecondChild {}

struct ConcreteFirstParent<T> : FirstParent {
  var firstChild: ConcreteChild { return ConcreteChild() }
}

struct ConcreteSecondParent<T> : SecondParent {
  var secondChild: ConcreteChild { return ConcreteChild() }
}

doStuff(f: ConcreteFirstParent<ConcreteChild>(),
        s: ConcreteSecondParent<ConcreteChild>())

// CHECK-LABEL: sil shared [noinline] @_TTSf4d_d___TTSg5GV31specialize_same_type_constraint19ConcreteFirstParentVS_13ConcreteChild_GS0_S1__S_11FirstParentS__GVS_20ConcreteSecondParentS1__GS3_S1__S_12SecondParentS____TF31specialize_same_type_constraint7doStuffu0_RxS_11FirstParent_S_12SecondParentwx5Childzw_5ChildrFT1fx1sq__T_
// CHECK: [[FIRST:%.*]] = function_ref @_TF31specialize_same_type_constraint15takesFirstChilduRxS_10FirstChildrFT1tx_T_
// CHECK: apply [[FIRST]]<ConcreteChild>({{.*}}) : $@convention(thin) <τ_0_0 where τ_0_0 : FirstChild> (@in τ_0_0) -> ()
// CHECK: [[SECOND:%.*]] = function_ref @_TF31specialize_same_type_constraint16takesSecondChilduRxS_11SecondChildrFT1tx_T_
// CHECK: apply [[SECOND]]<ConcreteChild>({{.*}}) : $@convention(thin) <τ_0_0 where τ_0_0 : SecondChild> (@in τ_0_0) -> ()
// CHECK: return

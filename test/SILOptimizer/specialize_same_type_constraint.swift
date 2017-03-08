// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests  -O -emit-sil -primary-file %s | %FileCheck %s

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

// CHECK-LABEL: sil shared [noinline] @_T031specialize_same_type_constraint7doStuffyx1f_q_1stAA11FirstParentRzAA06SecondH0R_5ChildQy_AGRtzr0_lFAA08ConcretegH0VyAA0kJ0VG_AA0kiH0VyAMGTg5Tf4dd_n : $@convention(thin) () -> () {
// CHECK: [[FIRST:%.*]] = function_ref @_T031specialize_same_type_constraint15takesFirstChildyx1t_tAA0fG0RzlF
// CHECK: apply [[FIRST]]<ConcreteChild>({{.*}}) : $@convention(thin) <τ_0_0 where τ_0_0 : FirstChild> (@in τ_0_0) -> ()
// CHECK: [[SECOND:%.*]] = function_ref @_T031specialize_same_type_constraint16takesSecondChildyx1t_tAA0fG0RzlF
// CHECK: apply [[SECOND]]<ConcreteChild>({{.*}}) : $@convention(thin) <τ_0_0 where τ_0_0 : SecondChild> (@in τ_0_0) -> ()
// CHECK: return

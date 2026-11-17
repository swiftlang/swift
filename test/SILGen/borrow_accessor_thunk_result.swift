// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-sil %s -module-name borrow_accessor_thunk_result -o /dev/null -Xllvm -sil-print-after=SILGenCleanup 2>&1 | %FileCheck %s --check-prefix=CHECK-SIL

public class Klass {
  public init() {}
}

public struct NonTrivial {
  public var k = Klass()
}

public protocol P {
  var nt: NonTrivial { borrow }
  var i: Int { borrow }
}

public struct S : P {
  var _nt: NonTrivial
  var _i: Int

  public var nt: NonTrivial {
    borrow {
      return _nt
    }
  }

  public var i: Int {
    borrow {
      return _i
    }
  }
}

open class Base {
  let _nt = NonTrivial()
  let _i = 0

  internal var nt: NonTrivial {
    borrow {
      return _nt
    }
  }

  internal var i: Int {
    borrow {
      return _i
    }
  }
}

// A public override of an internal method requires a vtable thunk.
open class Derived : Base {
  let _dnt = NonTrivial()
  let _di = 0

  public override var nt: NonTrivial {
    borrow {
      return _dnt
    }
  }

  public override var i: Int {
    borrow {
      return _di
    }
  }
}

// CHECK-LABEL: sil shared [transparent] [serialized] [thunk] [ossa] @$s28borrow_accessor_thunk_result1SVAA1PA2aDP2ntAA10NonTrivialVvbTW : $@convention(witness_method: P) (@in_guaranteed S) -> @guaranteed NonTrivial {
// CHECK:   [[SELF:%.*]] = load_borrow %0
// CHECK:   [[RES:%.*]] = apply {{%.*}}({{%.*}}) : $@convention(method) (@guaranteed S) -> @guaranteed NonTrivial
// CHECK:   [[RET:%.*]] = unchecked_ownership [[RES]]
// CHECK:   end_borrow [[RES]]
// CHECK:   end_borrow [[SELF]]
// CHECK:   return [[RET]]
// CHECK: } // end sil function '$s28borrow_accessor_thunk_result1SVAA1PA2aDP2ntAA10NonTrivialVvbTW'

// CHECK-LABEL: sil shared [transparent] [serialized] [thunk] [ossa] @$s28borrow_accessor_thunk_result1SVAA1PA2aDP1iSivbTW : $@convention(witness_method: P) (@in_guaranteed S) -> @guaranteed Int {
// CHECK:   [[SELF:%.*]] = load_borrow %0
// CHECK:   [[RES:%.*]] = apply {{%.*}}({{%.*}}) : $@convention(method) (@guaranteed S) -> @guaranteed Int
// CHECK-NOT: unchecked_ownership [[RES]]
// CHECK:   end_borrow [[SELF]]
// CHECK:   return [[RES]]
// CHECK: } // end sil function '$s28borrow_accessor_thunk_result1SVAA1PA2aDP1iSivbTW'

// CHECK-LABEL: sil private [thunk] [ossa] @$s28borrow_accessor_thunk_result7DerivedC2ntAA10NonTrivialVvbAA4BaseCAdFvbTV : $@convention(method) (@guaranteed Derived) -> @guaranteed NonTrivial {
// CHECK:   [[RES:%.*]] = apply {{%.*}}({{%.*}}) : $@convention(method) (@guaranteed Derived) -> @guaranteed NonTrivial
// CHECK:   [[RET:%.*]] = unchecked_ownership [[RES]]
// CHECK:   end_borrow [[RES]]
// CHECK:   return [[RET]]
// CHECK: } // end sil function '$s28borrow_accessor_thunk_result7DerivedC2ntAA10NonTrivialVvbAA4BaseCAdFvbTV'

// CHECK-LABEL: sil private [thunk] [ossa] @$s28borrow_accessor_thunk_result7DerivedC1iSivbAA4BaseCADSivbTV : $@convention(method) (@guaranteed Derived) -> @guaranteed Int {
// CHECK:   [[RES:%.*]] = apply {{%.*}}({{%.*}}) : $@convention(method) (@guaranteed Derived) -> @guaranteed Int
// CHECK-NEXT: return [[RES]]
// CHECK: } // end sil function '$s28borrow_accessor_thunk_result7DerivedC1iSivbAA4BaseCADSivbTV'

// CHECK-SIL-LABEL: sil shared [transparent] [serialized] [thunk] [ossa] @$s28borrow_accessor_thunk_result1SVAA1PA2aDP2ntAA10NonTrivialVvbTW : $@convention(witness_method: P) (@in_guaranteed S) -> @guaranteed NonTrivial {
// CHECK-SIL:   [[SELF:%.*]] = load_borrow %0
// CHECK-SIL:   [[RES:%.*]] = apply {{%.*}}([[SELF]]) : $@convention(method) (@guaranteed S) -> @guaranteed NonTrivial
// CHECK-SIL-NEXT: return_borrow [[RES]] from_scopes ([[SELF]], [[RES]])
// CHECK-SIL: } // end sil function '$s28borrow_accessor_thunk_result1SVAA1PA2aDP2ntAA10NonTrivialVvbTW'

// CHECK-SIL-LABEL: sil shared [transparent] [serialized] [thunk] [ossa] @$s28borrow_accessor_thunk_result1SVAA1PA2aDP1iSivbTW : $@convention(witness_method: P) (@in_guaranteed S) -> @guaranteed Int {
// CHECK-SIL:   [[SELF:%.*]] = load_borrow %0
// CHECK-SIL:   [[RES:%.*]] = apply {{%.*}}([[SELF]]) : $@convention(method) (@guaranteed S) -> @guaranteed Int
// CHECK-SIL-NEXT: end_borrow [[SELF]]
// CHECK-SIL-NEXT: return [[RES]]
// CHECK-SIL: } // end sil function '$s28borrow_accessor_thunk_result1SVAA1PA2aDP1iSivbTW'

// CHECK-SIL-LABEL: sil private [thunk] [ossa] @$s28borrow_accessor_thunk_result7DerivedC2ntAA10NonTrivialVvbAA4BaseCAdFvbTV : $@convention(method) (@guaranteed Derived) -> @guaranteed NonTrivial {
// CHECK-SIL:   [[RES:%.*]] = apply {{%.*}}(%0) : $@convention(method) (@guaranteed Derived) -> @guaranteed NonTrivial
// CHECK-SIL-NEXT: return_borrow [[RES]] from_scopes ([[RES]])
// CHECK-SIL: } // end sil function '$s28borrow_accessor_thunk_result7DerivedC2ntAA10NonTrivialVvbAA4BaseCAdFvbTV'

// CHECK-SIL-LABEL: sil private [thunk] [ossa] @$s28borrow_accessor_thunk_result7DerivedC1iSivbAA4BaseCADSivbTV : $@convention(method) (@guaranteed Derived) -> @guaranteed Int {
// CHECK-SIL:   [[RES:%.*]] = apply {{%.*}}(%0) : $@convention(method) (@guaranteed Derived) -> @guaranteed Int
// CHECK-SIL-NEXT: return [[RES]]
// CHECK-SIL: } // end sil function '$s28borrow_accessor_thunk_result7DerivedC1iSivbAA4BaseCADSivbTV'

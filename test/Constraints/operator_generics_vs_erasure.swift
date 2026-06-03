// RUN: %target-swift-emit-silgen %s | %FileCheck %s

func identity<T>(_ t: T) -> T { return t }

extension Error where Self: Equatable {
  // CHECK-LABEL: sil hidden [ossa] @$ss5ErrorP28operator_generics_vs_erasureSQRzrlE6equalsySbsAA_pF
  // CHECK: // function_ref static Error.== infix(_:_:)
  // CHECK: {{.*}} = function_ref @$ss5ErrorP28operator_generics_vs_erasureE2eeoiySbx_sAA_ptFZ
  // CHECK: } // end sil function '$ss5ErrorP28operator_generics_vs_erasureSQRzrlE6equalsySbsAA_pF'
  func equals(_ other: any Error) -> Bool {
    other == self
  }

  // CHECK-LABEL: sil private [ossa] @$ss5ErrorP28operator_generics_vs_erasureSQRzrlE14equalsWithCastySbsAA_pFSbxXEfU_
  // CHECK: {{.*}} = witness_method $Self, #Equatable."==" : <Self where Self : Equatable, Self : ~Copyable, Self : ~Escapable> (Self.Type) -> (borrowing Self, borrowing Self) -> Bool
  // CHECK: } // end sil function '$ss5ErrorP28operator_generics_vs_erasureSQRzrlE14equalsWithCastySbsAA_pFSbxXEfU_'
  func equalsWithCast(_ other: any Error) -> Bool {
    (other as? Self).map { $0 == self } ?? false
  }

  // CHECK-LABEL: sil private [ossa] @$ss5ErrorP28operator_generics_vs_erasureSQRzrlE18equalsWithIdentityySbsAA_pFSbxXEfU_
  // CHECK: {{.*}} = witness_method $Self, #Equatable."==" : <Self where Self : Equatable, Self : ~Copyable, Self : ~Escapable> (Self.Type) -> (borrowing Self, borrowing Self) -> Bool
  // CHECK: } // end sil function '$ss5ErrorP28operator_generics_vs_erasureSQRzrlE18equalsWithIdentityySbsAA_pFSbxXEfU_'
  func equalsWithIdentity(_ other: any Error) -> Bool {
    (other as? Self).map { $0 == identity(self) } ?? false
  }
}

extension Error {
  static func == (lhs: Self, rhs: any Error) -> Bool {
    false
  }
}

// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

// This file tests that the AST produced after fixing accessibility warnings
// is valid according to SILGen and the verifiers.

public struct PublicStruct {
  // CHECK-DAG: sil{{( \[.+\])*}} @_TFV22accessibility_warnings12PublicStructg9publicVarSi
  public var publicVar = 0
  // CHECK-DAG: sil hidden @_TFV22accessibility_warnings12PublicStructCfT_S0_
}

internal struct InternalStruct {
  public var publicVar = 0

  public private(set) var publicVarPrivateSet = 0

  public public(set) var publicVarPublicSet = 0

  // CHECK-DAG: sil hidden @_TFV22accessibility_warnings14InternalStructg16publicVarGetOnlySi
  public var publicVarGetOnly: Int { return 0 }

  // CHECK-DAG: sil hidden @_TFV22accessibility_warnings14InternalStructg15publicVarGetSetSi
  public var publicVarGetSet: Int { get { return 0 } set {} }

  // CHECK-DAG: sil hidden @_TFV22accessibility_warnings14InternalStructCfT_S0_
}

private struct PrivateStruct {
  public var publicVar = 0
  // CHECK-DAG: sil private @_TFV22accessibility_warningsP33_5D2F2E026754A901C0FF90C404896D0213PrivateStructCfT_S0_
}


extension PublicStruct {
  // CHECK-DAG: sil @_TFV22accessibility_warnings12PublicStructCfT1xSi_S0_
  public init(x: Int) { self.init() }

  // CHECK-DAG: sil @_TFV22accessibility_warnings12PublicStructg18publicVarExtensionSi
  public var publicVarExtension: Int { get { return 0 } set {} }
}

extension InternalStruct {
  // CHECK-DAG: sil hidden @_TFV22accessibility_warnings14InternalStructCfT1xSi_S0_
  public init(x: Int) { self.init() }

  // CHECK-DAG: sil hidden @_TFV22accessibility_warnings14InternalStructg18publicVarExtensionSi
  public var publicVarExtension: Int { get { return 0 } set {} }
}

extension PrivateStruct {
  // CHECK-DAG: sil private @_TFV22accessibility_warningsP33_5D2F2E026754A901C0FF90C404896D0213PrivateStructCfT1xSi_S0_
  public init(x: Int) { self.init() }

  // CHECK-DAG: sil private @_TFV22accessibility_warningsP33_5D2F2E026754A901C0FF90C404896D0213PrivateStructg18publicVarExtensionSi
  public var publicVarExtension: Int { get { return 0 } set {} }
}

public extension PublicStruct {
  // CHECK-DAG: sil @_TFV22accessibility_warnings12PublicStruct15extMemberPublicfT_T_
  public func extMemberPublic() {}
  // CHECK-DAG: sil private @_TFV22accessibility_warnings12PublicStructP33_5D2F2E026754A901C0FF90C404896D0213extImplPublicfT_T_
  private func extImplPublic() {}
}
internal extension PublicStruct {
  // CHECK-DAG: sil hidden @_TFV22accessibility_warnings12PublicStruct17extMemberInternalfT_T_
  public func extMemberInternal() {} // expected-warning {{declaring a public instance method in an internal extension}} {{3-9=internal}}
  // CHECK-DAG: sil private @_TFV22accessibility_warnings12PublicStructP33_5D2F2E026754A901C0FF90C404896D0215extImplInternalfT_T_
  private func extImplInternal() {}
}
private extension PublicStruct {
  // CHECK-DAG: sil private @_TFV22accessibility_warnings12PublicStructP33_5D2F2E026754A901C0FF90C404896D0216extMemberPrivatefT_T_
  public func extMemberPrivate() {} // expected-warning {{declaring a public instance method in a private extension}} {{3-9=private}}
  // CHECK-DAG: sil private @_TFV22accessibility_warnings12PublicStructP33_5D2F2E026754A901C0FF90C404896D0214extImplPrivatefT_T_
  private func extImplPrivate() {}
}

internal extension InternalStruct {
  // CHECK-DAG: sil hidden @_TFV22accessibility_warnings14InternalStruct17extMemberInternalfT_T_
  public func extMemberInternal() {} // expected-warning {{declaring a public instance method in an internal extension}} {{3-9=internal}}
  // CHECK-DAG: sil private @_TFV22accessibility_warnings14InternalStructP33_5D2F2E026754A901C0FF90C404896D0215extImplInternalfT_T_
  private func extImplInternal() {}
}
private extension InternalStruct {
  // CHECK-DAG: sil private @_TFV22accessibility_warnings14InternalStructP33_5D2F2E026754A901C0FF90C404896D0216extMemberPrivatefT_T_
  public func extMemberPrivate() {} // expected-warning {{declaring a public instance method in a private extension}} {{3-9=private}}
  // CHECK-DAG: sil private @_TFV22accessibility_warnings14InternalStructP33_5D2F2E026754A901C0FF90C404896D0214extImplPrivatefT_T_
  private func extImplPrivate() {}
}


private extension PrivateStruct {
  // CHECK-DAG: sil private @_TFV22accessibility_warningsP33_5D2F2E026754A901C0FF90C404896D0213PrivateStruct16extMemberPrivatefT_T_
  public func extMemberPrivate() {} // expected-warning {{declaring a public instance method in a private extension}} {{3-9=private}}
  // CHECK-DAG: sil private @_TFV22accessibility_warningsP33_5D2F2E026754A901C0FF90C404896D0213PrivateStruct14extImplPrivatefT_T_
  private func extImplPrivate() {}
}

public protocol PublicReadOnlyOperations {
  var size: Int { get }
  subscript (_: Int) -> Int { get }
}

internal struct PrivateSettersForReadOnlyInternal : PublicReadOnlyOperations {
  // CHECK-DAG: sil hidden{{( \[.+\])*}} @_TFV22accessibility_warnings33PrivateSettersForReadOnlyInternalg4sizeSi
  public private(set) var size = 0
  // CHECK-DAG: sil hidden @_TFV22accessibility_warnings33PrivateSettersForReadOnlyInternalg9subscriptFSiSi
  // CHECK-DAG: sil private @_TFV22accessibility_warnings33PrivateSettersForReadOnlyInternals9subscriptFSiSi
  internal private(set) subscript (_: Int) -> Int { // no-warning
    get { return 42 }
    set {}
  }
}


public class PublicClass {
  // CHECK-DAG: sil{{( \[.+\])*}} @_TFC22accessibility_warnings11PublicClassg9publicVarSi
  public var publicVar = 0
  // CHECK-DAG: sil hidden @_TFC22accessibility_warnings11PublicClasscfT_S0_
}

internal class InternalClass {
  // CHECK-DAG: sil hidden{{( \[.+\])*}} @_TFC22accessibility_warnings13InternalClassg9publicVarSi
  public var publicVar = 0

  // CHECK-DAG: sil hidden [transparent] @_TFC22accessibility_warnings13InternalClassg19publicVarPrivateSetSi
  public private(set) var publicVarPrivateSet = 0

  public public(set) var publicVarPublicSet = 0

  // CHECK-DAG: sil hidden @_TFC22accessibility_warnings13InternalClassg16publicVarGetOnlySi
  public var publicVarGetOnly: Int { return 0 }

  // CHECK-DAG: sil hidden @_TFC22accessibility_warnings13InternalClassg15publicVarGetSetSi
  public var publicVarGetSet: Int { get { return 0 } set {} }

  // CHECK-DAG: sil hidden @_TFC22accessibility_warnings13InternalClasscfT_S0_
}

private class PrivateClass {
  // CHECK-DAG: sil private{{( \[.+\])*}} @_TFC22accessibility_warningsP33_5D2F2E026754A901C0FF90C404896D0212PrivateClassg9publicVarSi
  public var publicVar = 0
  // CHECK-DAG: sil private @_TFC22accessibility_warningsP33_5D2F2E026754A901C0FF90C404896D0212PrivateClasscfT_S0_
}


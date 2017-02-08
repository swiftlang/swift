// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen %s | %FileCheck %s

// This file tests that the AST produced after fixing accessibility warnings
// is valid according to SILGen and the verifiers.

public struct PublicStruct {
  // CHECK-DAG: sil{{( \[.+\])*}} @_T022accessibility_warnings12PublicStructV9publicVarSifg
  public var publicVar = 0
  // CHECK-DAG: sil hidden @_T022accessibility_warnings12PublicStructVACycfC
}

internal struct InternalStruct {
  public var publicVar = 0

  public private(set) var publicVarPrivateSet = 0

  public public(set) var publicVarPublicSet = 0

  // CHECK-DAG: sil hidden @_T022accessibility_warnings14InternalStructV16publicVarGetOnlySifg
  public var publicVarGetOnly: Int { return 0 }

  // CHECK-DAG: sil hidden @_T022accessibility_warnings14InternalStructV15publicVarGetSetSifg
  public var publicVarGetSet: Int { get { return 0 } set {} }

  // CHECK-DAG: sil hidden @_T022accessibility_warnings14InternalStructVACycfC
}

private struct PrivateStruct {
  public var publicVar = 0
  // CHECK-DAG: sil private @_T022accessibility_warnings13PrivateStruct33_5D2F2E026754A901C0FF90C404896D02LLVADycfC
}


extension PublicStruct {
  // CHECK-DAG: sil @_T022accessibility_warnings12PublicStructVACSi1x_tcfC
  public init(x: Int) { self.init() }

  // CHECK-DAG: sil @_T022accessibility_warnings12PublicStructV18publicVarExtensionSifg
  public var publicVarExtension: Int { get { return 0 } set {} }
}

extension InternalStruct {
  // CHECK-DAG: sil hidden @_T022accessibility_warnings14InternalStructVACSi1x_tcfC
  public init(x: Int) { self.init() }

  // CHECK-DAG: sil hidden @_T022accessibility_warnings14InternalStructV18publicVarExtensionSifg
  public var publicVarExtension: Int { get { return 0 } set {} }
}

extension PrivateStruct {
  // CHECK-DAG: sil private @_T022accessibility_warnings13PrivateStruct33_5D2F2E026754A901C0FF90C404896D02LLVADSi1x_tcfC
  public init(x: Int) { self.init() }

  // CHECK-DAG: sil private @_T022accessibility_warnings13PrivateStruct33_5D2F2E026754A901C0FF90C404896D02LLV18publicVarExtensionSifg
  public var publicVarExtension: Int { get { return 0 } set {} }
}

public extension PublicStruct {
  // CHECK-DAG: sil @_T022accessibility_warnings12PublicStructV09extMemberC0yyF
  public func extMemberPublic() {}
  // CHECK-DAG: sil private @_T022accessibility_warnings12PublicStructV07extImplC033_5D2F2E026754A901C0FF90C404896D02LLyyF
  private func extImplPublic() {}
}
internal extension PublicStruct {
  // CHECK-DAG: sil hidden @_T022accessibility_warnings12PublicStructV17extMemberInternalyyF
  public func extMemberInternal() {} // expected-warning {{declaring a public instance method in an internal extension}} {{3-9=internal}}
  // CHECK-DAG: sil private @_T022accessibility_warnings12PublicStructV15extImplInternal33_5D2F2E026754A901C0FF90C404896D02LLyyF
  private func extImplInternal() {}
}
private extension PublicStruct {
  // CHECK-DAG: sil private @_T022accessibility_warnings12PublicStructV16extMemberPrivate33_5D2F2E026754A901C0FF90C404896D02LLyyF
  public func extMemberPrivate() {} // expected-warning {{declaring a public instance method in a private extension}} {{3-9=private}}
  // CHECK-DAG: sil private @_T022accessibility_warnings12PublicStructV14extImplPrivate33_5D2F2E026754A901C0FF90C404896D02LLyyF
  private func extImplPrivate() {}
}

internal extension InternalStruct {
  // CHECK-DAG: sil hidden @_T022accessibility_warnings14InternalStructV09extMemberC0yyF
  public func extMemberInternal() {} // expected-warning {{declaring a public instance method in an internal extension}} {{3-9=internal}}
  // CHECK-DAG: sil private @_T022accessibility_warnings14InternalStructV07extImplC033_5D2F2E026754A901C0FF90C404896D02LLyyF
  private func extImplInternal() {}
}
private extension InternalStruct {
  // CHECK-DAG: sil private @_T022accessibility_warnings14InternalStructV16extMemberPrivate33_5D2F2E026754A901C0FF90C404896D02LLyyF
  public func extMemberPrivate() {} // expected-warning {{declaring a public instance method in a private extension}} {{3-9=private}}
  // CHECK-DAG: sil private @_T022accessibility_warnings14InternalStructV14extImplPrivate33_5D2F2E026754A901C0FF90C404896D02LLyyF
  private func extImplPrivate() {}
}


private extension PrivateStruct {
  // CHECK-DAG: sil private @_T022accessibility_warnings13PrivateStruct33_5D2F2E026754A901C0FF90C404896D02LLV09extMemberC0yyF
  public func extMemberPrivate() {} // expected-warning {{declaring a public instance method in a private extension}} {{3-9=private}}
  // CHECK-DAG: sil private @_T022accessibility_warnings13PrivateStruct33_5D2F2E026754A901C0FF90C404896D02LLV07extImplC0yyF
  private func extImplPrivate() {}
}

public protocol PublicReadOnlyOperations {
  var size: Int { get }
  subscript (_: Int) -> Int { get }
}

internal struct PrivateSettersForReadOnlyInternal : PublicReadOnlyOperations {
  // CHECK-DAG: sil hidden{{( \[.+\])*}} @_T022accessibility_warnings33PrivateSettersForReadOnlyInternalV4sizeSifg
  public private(set) var size = 0
  // CHECK-DAG: sil hidden @_T022accessibility_warnings33PrivateSettersForReadOnlyInternalV9subscriptSiSicfg
  // CHECK-DAG: sil private @_T022accessibility_warnings33PrivateSettersForReadOnlyInternalV9subscriptSiSicfs
  internal private(set) subscript (_: Int) -> Int { // no-warning
    get { return 42 }
    set {}
  }
}


public class PublicClass {
  // CHECK-DAG: sil{{( \[.+\])*}} @_T022accessibility_warnings11PublicClassC9publicVarSifg
  public var publicVar = 0
  // CHECK-DAG: sil hidden @_T022accessibility_warnings11PublicClassCACycfc
}

internal class InternalClass {
  // CHECK-DAG: sil hidden{{( \[.+\])*}} @_T022accessibility_warnings13InternalClassC9publicVarSifg
  public var publicVar = 0

  // CHECK-DAG: sil hidden @_T022accessibility_warnings13InternalClassC19publicVarPrivateSetSifg
  public private(set) var publicVarPrivateSet = 0

  public public(set) var publicVarPublicSet = 0

  // CHECK-DAG: sil hidden @_T022accessibility_warnings13InternalClassC16publicVarGetOnlySifg
  public var publicVarGetOnly: Int { return 0 }

  // CHECK-DAG: sil hidden @_T022accessibility_warnings13InternalClassC15publicVarGetSetSifg
  public var publicVarGetSet: Int { get { return 0 } set {} }

  // CHECK-DAG: sil hidden @_T022accessibility_warnings13InternalClassCACycfc
}

private class PrivateClass {
  // CHECK-DAG: sil private{{( \[.+\])*}} @_T022accessibility_warnings12PrivateClass33_5D2F2E026754A901C0FF90C404896D02LLC9publicVarSifg
  public var publicVar = 0
  // CHECK-DAG: sil private @_T022accessibility_warnings12PrivateClass33_5D2F2E026754A901C0FF90C404896D02LLCADycfc
}


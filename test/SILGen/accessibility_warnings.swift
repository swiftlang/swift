// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-emit-silgen -enable-sil-ownership %s | %FileCheck %s

// This file tests that the AST produced after fixing accessibility warnings
// is valid according to SILGen and the verifiers.

public struct PublicStruct {
  // CHECK-DAG: sil{{( \[.+\])*}} @$S22accessibility_warnings12PublicStructV9publicVarSivg
  public var publicVar = 0
  // CHECK-DAG: sil hidden @$S22accessibility_warnings12PublicStructVACycfC
}

internal struct InternalStruct {
  public var publicVar = 0

  public private(set) var publicVarPrivateSet = 0

  public public(set) var publicVarPublicSet = 0

  // CHECK-DAG: sil hidden @$S22accessibility_warnings14InternalStructV16publicVarGetOnlySivg
  public var publicVarGetOnly: Int { return 0 }

  // CHECK-DAG: sil hidden @$S22accessibility_warnings14InternalStructV15publicVarGetSetSivg
  public var publicVarGetSet: Int { get { return 0 } set {} }

  // CHECK-DAG: sil hidden @$S22accessibility_warnings14InternalStructVACycfC
}

private struct PrivateStruct {
  public var publicVar = 0
  // CHECK-DAG: sil private @$S22accessibility_warnings13PrivateStruct33_5D2F2E026754A901C0FF90C404896D02LLVADycfC
}


extension PublicStruct {
  // CHECK-DAG: sil @$S22accessibility_warnings12PublicStructV1xACSi_tcfC
  public init(x: Int) { self.init() }

  // CHECK-DAG: sil @$S22accessibility_warnings12PublicStructV18publicVarExtensionSivg
  public var publicVarExtension: Int { get { return 0 } set {} }
}

extension InternalStruct {
  // CHECK-DAG: sil hidden @$S22accessibility_warnings14InternalStructV1xACSi_tcfC
  public init(x: Int) { self.init() }

  // CHECK-DAG: sil hidden @$S22accessibility_warnings14InternalStructV18publicVarExtensionSivg
  public var publicVarExtension: Int { get { return 0 } set {} }
}

extension PrivateStruct {
  // CHECK-DAG: sil private @$S22accessibility_warnings13PrivateStruct33_5D2F2E026754A901C0FF90C404896D02LLV1xADSi_tcfC
  public init(x: Int) { self.init() }

  // CHECK-DAG: sil private @$S22accessibility_warnings13PrivateStruct33_5D2F2E026754A901C0FF90C404896D02LLV18publicVarExtensionSivg
  public var publicVarExtension: Int { get { return 0 } set {} }
}

public extension PublicStruct {
  // CHECK-DAG: sil @$S22accessibility_warnings12PublicStructV09extMemberC0yyF
  public func extMemberPublic() {}
  // CHECK-DAG: sil private @$S22accessibility_warnings12PublicStructV07extImplC033_5D2F2E026754A901C0FF90C404896D02LLyyF
  private func extImplPublic() {}
}
internal extension PublicStruct {
  // CHECK-DAG: sil hidden @$S22accessibility_warnings12PublicStructV17extMemberInternalyyF
  public func extMemberInternal() {} // expected-warning {{declaring a public instance method in an internal extension}} {{3-9=internal}}
  // CHECK-DAG: sil private @$S22accessibility_warnings12PublicStructV15extImplInternal33_5D2F2E026754A901C0FF90C404896D02LLyyF
  private func extImplInternal() {}
}
private extension PublicStruct {
  // CHECK-DAG: sil private @$S22accessibility_warnings12PublicStructV16extMemberPrivate33_5D2F2E026754A901C0FF90C404896D02LLyyF
  public func extMemberPrivate() {} // expected-warning {{declaring a public instance method in a private extension}} {{3-9=fileprivate}}
  // CHECK-DAG: sil private @$S22accessibility_warnings12PublicStructV14extImplPrivate33_5D2F2E026754A901C0FF90C404896D02LLyyF
  private func extImplPrivate() {}
}

internal extension InternalStruct {
  // CHECK-DAG: sil hidden @$S22accessibility_warnings14InternalStructV09extMemberC0yyF
  public func extMemberInternal() {} // expected-warning {{declaring a public instance method in an internal extension}} {{3-9=internal}}
  // CHECK-DAG: sil private @$S22accessibility_warnings14InternalStructV07extImplC033_5D2F2E026754A901C0FF90C404896D02LLyyF
  private func extImplInternal() {}
}
private extension InternalStruct {
  // CHECK-DAG: sil private @$S22accessibility_warnings14InternalStructV16extMemberPrivate33_5D2F2E026754A901C0FF90C404896D02LLyyF
  public func extMemberPrivate() {} // expected-warning {{declaring a public instance method in a private extension}} {{3-9=fileprivate}}
  // CHECK-DAG: sil private @$S22accessibility_warnings14InternalStructV14extImplPrivate33_5D2F2E026754A901C0FF90C404896D02LLyyF
  private func extImplPrivate() {}
}


private extension PrivateStruct {
  // CHECK-DAG: sil private @$S22accessibility_warnings13PrivateStruct33_5D2F2E026754A901C0FF90C404896D02LLV09extMemberC0yyF
  public func extMemberPrivate() {} // expected-warning {{declaring a public instance method in a private extension}} {{3-9=fileprivate}}
  // CHECK-DAG: sil private @$S22accessibility_warnings13PrivateStruct33_5D2F2E026754A901C0FF90C404896D02LLV07extImplC0yyF
  private func extImplPrivate() {}
}

public protocol PublicReadOnlyOperations {
  var size: Int { get }
  subscript (_: Int) -> Int { get }
}

internal struct PrivateSettersForReadOnlyInternal : PublicReadOnlyOperations {
  // CHECK-DAG: sil hidden{{( \[.+\])*}} @$S22accessibility_warnings33PrivateSettersForReadOnlyInternalV4sizeSivg
  public private(set) var size = 0
  // CHECK-DAG: sil hidden @$S22accessibility_warnings33PrivateSettersForReadOnlyInternalVyS2icig
  // CHECK-DAG: sil private @$S22accessibility_warnings33PrivateSettersForReadOnlyInternalVyS2icis
  internal private(set) subscript (_: Int) -> Int { // no-warning
    get { return 42 }
    set {}
  }
}


public class PublicClass {
  // CHECK-DAG: sil{{( \[.+\])*}} @$S22accessibility_warnings11PublicClassC9publicVarSivg
  public var publicVar = 0
  // CHECK-DAG: sil hidden @$S22accessibility_warnings11PublicClassCACycfc
}

internal class InternalClass {
  // CHECK-DAG: sil hidden{{( \[.+\])*}} @$S22accessibility_warnings13InternalClassC9publicVarSivg
  public var publicVar = 0

  // CHECK-DAG: sil hidden [transparent] @$S22accessibility_warnings13InternalClassC19publicVarPrivateSetSivg
  public private(set) var publicVarPrivateSet = 0

  public public(set) var publicVarPublicSet = 0

  // CHECK-DAG: sil hidden @$S22accessibility_warnings13InternalClassC16publicVarGetOnlySivg
  public var publicVarGetOnly: Int { return 0 }

  // CHECK-DAG: sil hidden @$S22accessibility_warnings13InternalClassC15publicVarGetSetSivg
  public var publicVarGetSet: Int { get { return 0 } set {} }

  // CHECK-DAG: sil hidden @$S22accessibility_warnings13InternalClassCACycfc
}

private class PrivateClass {
  // CHECK-DAG: sil private{{( \[.+\])*}} @$S22accessibility_warnings12PrivateClass33_5D2F2E026754A901C0FF90C404896D02LLC9publicVarSivg
  public var publicVar = 0
  // CHECK-DAG: sil private @$S22accessibility_warnings12PrivateClass33_5D2F2E026754A901C0FF90C404896D02LLCADycfc
}


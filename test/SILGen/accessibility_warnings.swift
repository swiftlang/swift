// RUN: %target-typecheck-verify-swift -package-name accessibility_warnings
// RUN: %target-swift-emit-silgen %s -package-name accessibility_warnings -module-name accessibility_warnings | %FileCheck %s -check-prefixes=CHECK,CHECK-NONRES
// RUN: %target-swift-emit-silgen %s -package-name accessibility_warnings -module-name accessibility_warnings -enable-library-evolution | %FileCheck %s -check-prefixes=CHECK,CHECK-RES

// This file tests that the AST produced after fixing accessibility warnings
// is valid according to SILGen and the verifiers.

public struct PublicStruct {
  // CHECK-RES-DAG: sil hidden [transparent] [ossa] @$s22accessibility_warnings12PublicStructV9publicVarSivpfi
  // CHECK-NONRES-DAG: sil [transparent] [ossa] @$s22accessibility_warnings12PublicStructV9publicVarSivpfi
  // CHECK-RES-DAG: sil [ossa] @$s22accessibility_warnings12PublicStructV9publicVarSivg
  // CHECK-NONRES-DAG: sil [transparent] [serialized] [ossa] @$s22accessibility_warnings12PublicStructV9publicVarSivg
  // CHECK-RES-DAG: sil [ossa] @$s22accessibility_warnings12PublicStructV9publicVarSivs
  // CHECK-NONRES-DAG: sil [transparent] [serialized] [ossa] @$s22accessibility_warnings12PublicStructV9publicVarSivs
  public var publicVar = 0
  // CHECK-DAG: sil hidden [ossa] @$s22accessibility_warnings12PublicStructVACycfC
}

package struct PackageStruct {
  // CHECK-RES-DAG: sil hidden [transparent] [ossa] @$s22accessibility_warnings13PackageStructV9publicVarSivpfi
  // CHECK-NONRES-DAG: sil package [transparent] [ossa] @$s22accessibility_warnings13PackageStructV9publicVarSivpfi
  // CHECK-RES-DAG: sil package [ossa] @$s22accessibility_warnings13PackageStructV9publicVarSivg
  // CHECK-NONRES-DAG: sil package [transparent] [ossa] @$s22accessibility_warnings13PackageStructV9publicVarSivg
  // CHECK-RES-DAG: sil package [ossa] @$s22accessibility_warnings13PackageStructV9publicVarSivs
  // CHECK-NONRES-DAG: sil package [transparent] [ossa] @$s22accessibility_warnings13PackageStructV9publicVarSivs :
  package var publicVar = 0
  // CHECK-DAG: sil hidden [ossa] @$s22accessibility_warnings13PackageStructVACycfC
}

internal struct InternalStruct {
  public var publicVar = 0

  public private(set) var publicVarPrivateSet = 0

  // expected-warning@+1 {{'public(set)' modifier is redundant for a public property}} {{10-22=}}
  public public(set) var publicVarPublicSet = 0

  // CHECK-DAG: sil hidden [ossa] @$s22accessibility_warnings14InternalStructV16publicVarGetOnlySivg
  public var publicVarGetOnly: Int { return 0 }

  // CHECK-DAG: sil hidden [ossa] @$s22accessibility_warnings14InternalStructV15publicVarGetSetSivg
  public var publicVarGetSet: Int { get { return 0 } set {} }

  // CHECK-DAG: sil hidden [ossa] @$s22accessibility_warnings14InternalStructVACycfC
}

private struct PrivateStruct {
  public var publicVar = 0
  // CHECK-DAG: sil private [ossa] @$s22accessibility_warnings13PrivateStruct33_5D2F2E026754A901C0FF90C404896D02LLVADycfC
}


extension PublicStruct {
  // CHECK-DAG: sil [ossa] @$s22accessibility_warnings12PublicStructV1xACSi_tcfC
  public init(x: Int) { self.init() }

  // CHECK-DAG: sil [ossa] @$s22accessibility_warnings12PublicStructV18publicVarExtensionSivg
  public var publicVarExtension: Int { get { return 0 } set {} }
}

extension PackageStruct {
  // CHECK-DAG: sil package [ossa] @$s22accessibility_warnings13PackageStructV1xACSi_tcfC : $@convention(method) (
  public init(x: Int) { self.init() }
  // CHECK-DAG: sil package [ossa] @$s22accessibility_warnings13PackageStructV18publicVarExtensionSivg
  public var publicVarExtension: Int { get { return 0 } set {} }
}

extension InternalStruct {
  // CHECK-DAG: sil hidden [ossa] @$s22accessibility_warnings14InternalStructV1xACSi_tcfC
  public init(x: Int) { self.init() }

  // CHECK-DAG: sil hidden [ossa] @$s22accessibility_warnings14InternalStructV18publicVarExtensionSivg
  public var publicVarExtension: Int { get { return 0 } set {} }
}

extension PrivateStruct {
  // CHECK-DAG: sil private [ossa] @$s22accessibility_warnings13PrivateStruct33_5D2F2E026754A901C0FF90C404896D02LLV1xADSi_tcfC
  public init(x: Int) { self.init() }

  // CHECK-DAG: sil private [ossa] @$s22accessibility_warnings13PrivateStruct33_5D2F2E026754A901C0FF90C404896D02LLV18publicVarExtensionSivg
  public var publicVarExtension: Int { get { return 0 } set {} }
}

public extension PublicStruct {
  // CHECK-DAG: sil [ossa] @$s22accessibility_warnings12PublicStructV09extMemberC0yyF
  public func extMemberPublic() {} // expected-warning {{'public' modifier is redundant for instance method declared in a public extension}} {{3-10=}}
  // CHECK-DAG: sil private [ossa] @$s22accessibility_warnings12PublicStructV07extImplC033_5D2F2E026754A901C0FF90C404896D02LLyyF
  private func extImplPublic() {}
}

package extension PublicStruct {
  // CHECK-DAG: sil package [ossa] @$s22accessibility_warnings12PublicStructV16extMemberPackageyyF
  func extMemberPackage() {}
  // CHECK-DAG: sil hidden [ossa] @$s22accessibility_warnings12PublicStructV16extMethodPackageyyF
  internal func extMethodPackage() {}
  // CHECK-DAG: sil private [ossa] @$s22accessibility_warnings12PublicStructV14extImplPackage33_5D2F2E026754A901C0FF90C404896D02LLyyF
  private func extImplPackage() {}
}

internal extension PublicStruct {
  // CHECK-DAG: sil [ossa] @$s22accessibility_warnings12PublicStructV17extMemberInternalyyF
  public func extMemberInternal() {} // expected-warning {{'public' modifier conflicts with extension's default access of 'internal'}} {{none}}
  // CHECK-DAG: sil private [ossa] @$s22accessibility_warnings12PublicStructV15extImplInternal33_5D2F2E026754A901C0FF90C404896D02LLyyF
  private func extImplInternal() {}
}
private extension PublicStruct {
  // CHECK-DAG: sil [ossa] @$s22accessibility_warnings12PublicStructV16extMemberPrivateyyF
  public func extMemberPrivate() {} // expected-warning {{'public' modifier conflicts with extension's default access of 'private'}} {{none}}
  // CHECK-DAG: sil private [ossa] @$s22accessibility_warnings12PublicStructV14extImplPrivate33_5D2F2E026754A901C0FF90C404896D02LLyyF
  private func extImplPrivate() {}
}

package extension PackageStruct {
  // CHECK-DAG: sil package [ossa] @$s22accessibility_warnings13PackageStructV09extMemberC0yyF
  func extMemberPackage() {}
  // CHECK-DAG: sil hidden [ossa] @$s22accessibility_warnings13PackageStructV09extMethodC0yyF
  internal func extMethodPackage() {}
  // CHECK-DAG: sil private [ossa] @$s22accessibility_warnings13PackageStructV07extImplC033_5D2F2E026754A901C0FF90C404896D02LLyyF
  private func extImplPackage() {}
}

internal extension InternalStruct {
  // CHECK-DAG: sil hidden [ossa] @$s22accessibility_warnings14InternalStructV09extMemberC0yyF
  public func extMemberInternal() {} // expected-warning {{'public' modifier conflicts with extension's default access of 'internal'}} {{none}}
  // CHECK-DAG: sil private [ossa] @$s22accessibility_warnings14InternalStructV07extImplC033_5D2F2E026754A901C0FF90C404896D02LLyyF
  private func extImplInternal() {}
}
private extension InternalStruct {
  // CHECK-DAG: sil hidden [ossa] @$s22accessibility_warnings14InternalStructV16extMemberPrivateyyF
  public func extMemberPrivate() {} // expected-warning {{'public' modifier conflicts with extension's default access of 'private'}} {{none}}
  // CHECK-DAG: sil private [ossa] @$s22accessibility_warnings14InternalStructV14extImplPrivate33_5D2F2E026754A901C0FF90C404896D02LLyyF
  private func extImplPrivate() {}
}


private extension PrivateStruct {
  // CHECK-DAG: sil private [ossa] @$s22accessibility_warnings13PrivateStruct33_5D2F2E026754A901C0FF90C404896D02LLV09extMemberC0yyF
  public func extMemberPrivate() {} // expected-warning {{'public' modifier conflicts with extension's default access of 'private'}} {{none}}
  // CHECK-DAG: sil private [ossa] @$s22accessibility_warnings13PrivateStruct33_5D2F2E026754A901C0FF90C404896D02LLV07extImplC0yyF
  private func extImplPrivate() {}
}

public protocol PublicReadOnlyOperations {
  var size: Int { get }
  subscript (_: Int) -> Int { get }
}

internal struct PrivateSettersForReadOnlyInternal : PublicReadOnlyOperations {
  // CHECK-DAG: sil hidden{{( \[.+\])*}} [ossa] @$s22accessibility_warnings33PrivateSettersForReadOnlyInternalV4sizeSivg
  public private(set) var size = 0
  // CHECK-DAG: sil hidden [ossa] @$s22accessibility_warnings33PrivateSettersForReadOnlyInternalVyS2icig
  // CHECK-DAG: sil hidden [ossa] @$s22accessibility_warnings33PrivateSettersForReadOnlyInternalVyS2icis
  internal private(set) subscript (_: Int) -> Int { // no-warning
    get { return 42 }
    set {}
  }
}

public class PublicClass {
  // CHECK-RES-DAG: sil [ossa] @$s22accessibility_warnings11PublicClassC9publicVarSivg
  // CHECK-NONRES-DAG: sil [transparent] [serialized] [ossa] @$s22accessibility_warnings11PublicClassC9publicVarSivg
  // CHECK-RES-DAG: sil [ossa] @$s22accessibility_warnings11PublicClassC9publicVarSivs
  // CHECK-NONRES-DAG: sil [transparent] [serialized] [ossa] @$s22accessibility_warnings11PublicClassC9publicVarSivs
  public var publicVar = 0

  // CHECK-RES-DAG: sil [ossa] @$s22accessibility_warnings11PublicClassC19publicVarPrivateSetSivg
  // CHECK-NONRES-DAG: sil [transparent] [serialized] [ossa] @$s22accessibility_warnings11PublicClassC19publicVarPrivateSetSivg
  // CHECK-RES-DAG: sil hidden [ossa] @$s22accessibility_warnings11PublicClassC19publicVarPrivateSetSivs
  // CHECK-NONRES-DAG: sil hidden [transparent] [ossa] @$s22accessibility_warnings11PublicClassC19publicVarPrivateSetSivs
  public private(set) var publicVarPrivateSet = 0

  // CHECK-RES-DAG: sil [ossa] @$s22accessibility_warnings11PublicClassC20publicVarInternalSetSivg 
  // CHECK-NONRES-DAG: sil [transparent] [serialized] [ossa] @$s22accessibility_warnings11PublicClassC20publicVarInternalSetSivg
  // CHECK-RES-DAG: sil hidden [ossa] @$s22accessibility_warnings11PublicClassC20publicVarInternalSetSivs
  // CHECK-NONRES-DAG: sil hidden [transparent] [ossa] @$s22accessibility_warnings11PublicClassC20publicVarInternalSetSivs
  public internal(set) var publicVarInternalSet = 0

  // CHECK-RES-DAG: sil [ossa] @$s22accessibility_warnings11PublicClassC19publicVarPackageSetSivg
  // CHECK-NONRES-DAG: sil [transparent] [serialized] [ossa] @$s22accessibility_warnings11PublicClassC19publicVarPackageSetSivg
  // CHECK-RES-DAG: sil package [ossa] @$s22accessibility_warnings11PublicClassC19publicVarPackageSetSivs
  // CHECK-NONRES-DAG: sil package [transparent] [ossa] @$s22accessibility_warnings11PublicClassC19publicVarPackageSetSivs
  public package(set) var publicVarPackageSet = 0

  // CHECK-DAG: sil [ossa] @$s22accessibility_warnings11PublicClassC16publicVarGetOnlySivg
  public var publicVarGetOnly: Int { return 0 }

  // CHECK-DAG: sil [ossa] @$s22accessibility_warnings11PublicClassC15publicVarGetSetSivg
  // CHECK-DAG: sil [ossa] @$s22accessibility_warnings11PublicClassC15publicVarGetSetSivs
  public var publicVarGetSet: Int { get { return 0 } set {} }

  // CHECK-DAG: sil hidden{{( \[.+\])*}} [ossa] @$s22accessibility_warnings11PublicClassCACycfc
}

internal class InternalClass {
  // CHECK-DAG: sil hidden [transparent] [ossa] @$s22accessibility_warnings13InternalClassC9publicVarSivg
  // CHECK-DAG: sil hidden [transparent] [ossa] @$s22accessibility_warnings13InternalClassC9publicVarSivs
  public var publicVar = 0

  // CHECK-DAG: sil hidden [transparent] [ossa] @$s22accessibility_warnings13InternalClassC19publicVarPrivateSetSivg
  // CHECK-DAG: sil hidden [transparent] [ossa] @$s22accessibility_warnings13InternalClassC19publicVarPrivateSetSivs
  public private(set) var publicVarPrivateSet = 0

  // expected-warning@+1 {{'public(set)' modifier is redundant for a public property}} {{10-22=}}
  public public(set) var publicVarPublicSet = 0

  // CHECK-DAG: sil hidden [ossa] @$s22accessibility_warnings13InternalClassC16publicVarGetOnlySivg
  public var publicVarGetOnly: Int { return 0 }

  // CHECK-DAG: sil hidden [transparent] [ossa] @$s22accessibility_warnings13InternalClassC18publicVarPublicSetSivg
  // CHECK-DAG: sil hidden [transparent] [ossa] @$s22accessibility_warnings13InternalClassC18publicVarPublicSetSivs
  public var publicVarGetSet: Int { get { return 0 } set {} }

  // CHECK-DAG: sil hidden{{( \[.+\])*}} [ossa] @$s22accessibility_warnings13InternalClassCACycfc
}

package class PackageClass {
  // CHECK-RES-DAG: sil package [ossa] @$s22accessibility_warnings12PackageClassC9publicVarSivg
  // CHECK-RES-DAG: sil package [ossa] @$s22accessibility_warnings12PackageClassC9publicVarSivs
  // CHECK-NONRES-DAG: sil package [transparent] [ossa] @$s22accessibility_warnings12PackageClassC9publicVarSivg
  // CHECK-NONRES-DAG: sil package [transparent] [ossa] @$s22accessibility_warnings12PackageClassC9publicVarSivs
  public var publicVar = 0

  // CHECK-RES-DAG: sil package [ossa] @$s22accessibility_warnings12PackageClassC19publicVarPrivateSetSivg
  // CHECK-RES-DAG: sil hidden [ossa] @$s22accessibility_warnings12PackageClassC19publicVarPrivateSetSivs
  // CHECK-NONRES-DAG: sil package [transparent] [ossa] @$s22accessibility_warnings12PackageClassC19publicVarPrivateSetSivg
  // CHECK-NONRES-DAG: sil hidden [transparent] [ossa] @$s22accessibility_warnings12PackageClassC19publicVarPrivateSetSivs
  public private(set) var publicVarPrivateSet = 0

  // expected-warning@+1 {{'public(set)' modifier is redundant for a public property}} {{10-22=}}
  public public(set) var publicVarPublicSet = 0

  // CHECK-RES-DAG: sil package [ossa] @$s22accessibility_warnings12PackageClassC18publicVarPublicSetSivg
  // CHECK-NONRES-DAG: sil package [ossa] @$s22accessibility_warnings12PackageClassC16publicVarGetOnlySivg
  public var publicVarGetOnly: Int { return 0 }

  // CHECK-DAG: sil package [ossa] @$s22accessibility_warnings12PackageClassC15publicVarGetSetSivg
  // CHECK-DAG: sil package [ossa] @$s22accessibility_warnings12PackageClassC15publicVarGetSetSivs
  public var publicVarGetSet: Int { get { return 0 } set {} }

  // CHECK-DAG: sil hidden{{( \[.+\])*}} [ossa] @$s22accessibility_warnings12PackageClassCACycfc
}

private class PrivateClass {
  //CHECK-DAG: sil private [transparent] [ossa] @$s22accessibility_warnings12PrivateClass33_5D2F2E026754A901C0FF90C404896D02LLC9publicVarSivg
  // CHECK-DAG: sil private [transparent] [ossa] @$s22accessibility_warnings12PrivateClass33_5D2F2E026754A901C0FF90C404896D02LLC9publicVarSivs
  public var publicVar = 0
  // CHECK-DAG: sil private{{( \[.+\])*}} [ossa] @$s22accessibility_warnings12PrivateClass33_5D2F2E026754A901C0FF90C404896D02LLCADycfc
}


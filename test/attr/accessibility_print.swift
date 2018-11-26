// RUN: %empty-directory(%t)
// RUN: %target-swift-ide-test -skip-deinit=false -print-ast-typechecked -print-access -source-filename=%s | %FileCheck %s -check-prefix=CHECK -check-prefix=CHECK-SRC
// RUN: %target-swift-frontend -emit-module-path %t/accessibility_print.swiftmodule %s
// RUN: %target-swift-ide-test -skip-deinit=false -print-module -print-access -module-to-print=accessibility_print -I %t -source-filename=%s | %FileCheck %s

// This file uses alphabetic prefixes on its declarations because swift-ide-test
// sorts decls in a module before printing them.

// CHECK-LABEL: internal var AA_defaultGlobal
var AA_defaultGlobal = 0

// CHECK: {{^}}private{{(\*/)?}} var AB_privateGlobal
// CHECK: {{^}}internal{{(\*/)?}} var AC_internalGlobal
// CHECK: {{^}}public{{(\*/)?}} var AD_publicGlobal
// CHECK: {{^}}fileprivate{{(\*/)?}} var AE_fileprivateGlobal
private var AB_privateGlobal = 0
internal var AC_internalGlobal = 0
public var AD_publicGlobal = 0
fileprivate var AE_fileprivateGlobal = 0


// CHECK-LABEL: internal struct BA_DefaultStruct {
struct BA_DefaultStruct {
  // CHECK: internal let x
  let x = 0
} // CHECK: {{^[}]}}

// CHECK-LABEL: private{{(\*/)?}} struct BB_PrivateStruct {
private struct BB_PrivateStruct {
  // CHECK: internal var x
  var x = 0
  // CHECK: internal init(x: Int)
  // CHECK: internal init()
} // CHECK: {{^[}]}}

// CHECK-LABEL: internal{{(\*/)?}} struct BC_InternalStruct {
internal struct BC_InternalStruct {
  // CHECK: internal let x
  let x = 0
  // CHECK: internal init()
} // CHECK: {{^[}]}}

// CHECK-LABEL: public{{(\*/)?}} struct BD_PublicStruct {
public struct BD_PublicStruct {
  // CHECK: internal var x
  var x = 0
  // CHECK: internal init(x: Int)
  // CHECK: internal init()
} // CHECK: {{^[}]}}

// CHECK-LABEL: public{{(\*/)?}} struct BE_PublicStructPrivateMembers {
public struct BE_PublicStructPrivateMembers {
  // CHECK: private{{(\*/)?}} var x
  private var x = 0
  // CHECK: private init(x: Int)
  // CHECK: internal init()
} // CHECK: {{^[}]}}

// CHECK-LABEL: {{^}}fileprivate{{(\*/)?}} struct BF_FilePrivateStruct {
fileprivate struct BF_FilePrivateStruct {
  // CHECK: {{^}} internal var x
  var x = 0
  // CHECK: {{^}} internal init(x: Int)
  // CHECK: {{^}} internal init()
} // CHECK: {{^[}]}}


// CHECK-LABEL: private{{(\*/)?}} class CA_PrivateClass
private class CA_PrivateClass {
  // CHECK: {{^}} deinit
  deinit {}
  // CHECK: internal init()
} // CHECK: {{^[}]}}

// CHECK-LABEL: internal{{(\*/)?}} class CB_InternalClass
internal class CB_InternalClass {
  // CHECK: {{^}} deinit
  deinit {}
  // CHECK: internal init()
} // CHECK: {{^[}]}}

// CHECK-LABEL: public{{(\*/)?}} class CC_PublicClass
public class CC_PublicClass {
  // CHECK: {{^}} deinit
  deinit {}
  // CHECK: internal init()
} // CHECK: {{^[}]}}


// CHECK-LABEL: private{{(\*/)?}} enum DA_PrivateEnum {
private enum DA_PrivateEnum {
  // CHECK: {{^}} case Foo
  // CHECK: Bar
  case Foo, Bar
  // CHECK: internal init()
  init() { self = .Foo }
  // CHECK: private var hashValue
} // CHECK: {{^[}]}}

// CHECK-LABEL: internal{{(\*/)?}} enum DB_InternalEnum {
internal enum DB_InternalEnum {
  // CHECK: {{^}} case Foo
  // CHECK: Bar
  case Foo, Bar
  // CHECK: internal init()
  init() { self = .Foo }
  // CHECK: internal var hashValue
} // CHECK: {{^[}]}}

// CHECK-LABEL: public{{(\*/)?}} enum DC_PublicEnum {
public enum DC_PublicEnum {
  // CHECK: {{^}} case Foo
  // CHECK: Bar
  case Foo, Bar
  // CHECK: internal init()
  init() { self = .Foo }
  // CHECK: public var hashValue
} // CHECK: {{^[}]}}


// CHECK-LABEL: private{{(\*/)?}} protocol EA_PrivateProtocol {
private protocol EA_PrivateProtocol {
  // CHECK: {{^}} associatedtype Foo
  associatedtype Foo
  // CHECK: {{^}} var Bar
  var Bar: Int { get }
  // CHECK: {{^}} func baz()
  func baz()
} // CHECK: {{^[}]}}

// CHECK-LABEL: public{{(\*/)?}} protocol EB_PublicProtocol {
public protocol EB_PublicProtocol {
  // CHECK: {{^}} associatedtype Foo
  associatedtype Foo
  // CHECK: {{^}} var Bar
  var Bar: Int { get }
  // CHECK: {{^}} func baz()
  func baz()
} // CHECK: {{^[}]}}


private class FA_PrivateClass {}
internal class FB_InternalClass {}
public class FC_PublicClass {}
// CHECK-SRC: {{^}}ex
// CHECK-LABEL: tension FA_PrivateClass {
extension FA_PrivateClass {
  // CHECK: internal func a()
  func a() {}
} // CHECK: {{^[}]}}

// CHECK-LABEL: extension FB_InternalClass {
extension FB_InternalClass {
  // CHECK: internal func a()
  func a() {}
} // CHECK: {{^[}]}}

// CHECK-LABEL: extension FC_PublicClass {
extension FC_PublicClass {
  // CHECK: internal func a()
  func a() {}
} // CHECK: {{^[}]}}


private class FD_PrivateClass {}
// CHECK-SRC: private
// CHECK-LABEL: extension FD_PrivateClass {
private extension FD_PrivateClass {
  // CHECK: private func explicitPrivateExt()
  func explicitPrivateExt() {}
} // CHECK: {{^[}]}}


public class FE_PublicClass {}
// CHECK-SRC: private
// CHECK-LABEL: extension FE_PublicClass {
private extension FE_PublicClass {
  // CHECK: private func explicitPrivateExt()
  func explicitPrivateExt() {}
  // CHECK: private struct PrivateNested {
  struct PrivateNested {
    // CHECK: internal var x
    var x: Int
  } // CHECK: }
} // CHECK: {{^[}]}}

// CHECK-SRC: internal
// CHECK-LABEL: extension FE_PublicClass {
internal extension FE_PublicClass {
  // CHECK: internal func explicitInternalExt()
  func explicitInternalExt() {}
  // CHECK: internal struct InternalNested {
  struct InternalNested {
    // CHECK: internal var x
    var x: Int
  } // CHECK: }
} // CHECK: {{^[}]}}

// CHECK-SRC: public
// CHECK-LABEL: extension FE_PublicClass {
public extension FE_PublicClass {
  // CHECK: public func explicitPublicExt()
  func explicitPublicExt() {}
  // CHECK: public struct PublicNested {
  struct PublicNested {
    // CHECK: internal var x
    var x: Int
  } // CHECK: }
} // CHECK: {{^[}]}}


// CHECK-LABEL: internal func GA_localTypes()
func GA_localTypes() {
  // CHECK-SRC: private struct Local {
  struct Local {
    // CHECK-SRC: internal let x
    let x = 0
  }
  _ = Local()

  // CHECK-SRC: private enum LocalEnum {
  enum LocalEnum {
    // CHECK-SRC: {{^}} case A
    case A, B
  }
  let enumVal = LocalEnum.A
  _ = (enumVal == .B)
} // CHECK-SRC: {{^[}]}}

// CHECK-LABEL: public{{(\*/)?}} struct GB_NestedOuter {
public struct GB_NestedOuter {
  // CHECK: internal struct Inner {
  struct Inner {
    // CHECK: private{{(\*/)?}} let x
    private let x = 0
    // CHECK: internal let y
    let y = 0
  }
} // CHECK: {{^[}]}}

// CHECK-LABEL: private{{(\*/)?}} struct GC_NestedOuterPrivate {
private struct GC_NestedOuterPrivate {
  // CHECK: internal struct Inner {
  struct Inner {
    // CHECK: private{{(\*/)?}} let x
    private let x = 0
    // CHECK: internal let y
    let y = 0
  }
} // CHECK: {{^[}]}}


public protocol HA_PublicProtocol {
  associatedtype Assoc
}
internal protocol HB_InternalProtocol {
  associatedtype Assoc
}
private protocol HC_PrivateProtocol {
  associatedtype Assoc
}
public struct HA_PublicStruct {}
internal struct HB_InternalStruct {}
private struct HC_PrivateStruct {}

// CHECK-LABEL: extension HA_PublicProtocol {
extension HA_PublicProtocol {
  // CHECK: internal func unconstrained()
  func unconstrained() {}
} // CHECK: {{^[}]}}

// CHECK-LABEL: extension HA_PublicProtocol where Self.Assoc == HA_PublicStruct {
extension HA_PublicProtocol where Assoc == HA_PublicStruct {
  // CHECK: internal func constrained()
  func constrained() {}
} // CHECK: {{^[}]}}
// CHECK-LABEL: extension HA_PublicProtocol where Self.Assoc == HB_InternalStruct {
extension HA_PublicProtocol where Assoc == HB_InternalStruct {
  // CHECK: internal func constrained()
  func constrained() {}
} // CHECK: {{^[}]}}
// CHECK-LABEL: extension HA_PublicProtocol where Self.Assoc == HC_PrivateStruct {
extension HA_PublicProtocol where Assoc == HC_PrivateStruct {
  // CHECK: private func constrained()
  func constrained() {}
} // CHECK: {{^[}]}}
// CHECK-LABEL: extension HB_InternalProtocol {
extension HB_InternalProtocol {
  // CHECK: internal func unconstrained()
  func unconstrained() {}
} // CHECK: {{^[}]}}
// CHECK-LABEL: extension HB_InternalProtocol where Self.Assoc == HA_PublicStruct {
extension HB_InternalProtocol where Assoc == HA_PublicStruct {
  // CHECK: internal func constrained()
  func constrained() {}
} // CHECK: {{^[}]}}
// CHECK-LABEL: extension HB_InternalProtocol where Self.Assoc == HB_InternalStruct {
extension HB_InternalProtocol where Assoc == HB_InternalStruct {
  // CHECK: internal func constrained()
  func constrained() {}
} // CHECK: {{^[}]}}
// CHECK-LABEL: extension HB_InternalProtocol where Self.Assoc == HC_PrivateStruct {
extension HB_InternalProtocol where Assoc == HC_PrivateStruct {
  // CHECK: private func constrained()
  func constrained() {}
} // CHECK: {{^[}]}}
// CHECK-LABEL: extension HC_PrivateProtocol {
extension HC_PrivateProtocol {
  // CHECK: internal func unconstrained()
  func unconstrained() {}
} // CHECK: {{^[}]}}
// CHECK-LABEL: extension HC_PrivateProtocol where Self.Assoc == HA_PublicStruct {
extension HC_PrivateProtocol where Assoc == HA_PublicStruct {
  // CHECK: private func constrained()
  func constrained() {}
} // CHECK: {{^[}]}}
// CHECK-LABEL: extension HC_PrivateProtocol where Self.Assoc == HB_InternalStruct {
extension HC_PrivateProtocol where Assoc == HB_InternalStruct {
  // CHECK: private func constrained()
  func constrained() {}
} // CHECK: {{^[}]}}
// CHECK-LABEL: extension HC_PrivateProtocol where Self.Assoc == HC_PrivateStruct {
extension HC_PrivateProtocol where Assoc == HC_PrivateStruct {
  // CHECK: private func constrained()
  func constrained() {}
} // CHECK: {{^[}]}}

public protocol IA_PublicAssocTypeProto {
  associatedtype PublicValue
  var publicValue: PublicValue { get }
}
fileprivate protocol IB_FilePrivateAssocTypeProto {
  associatedtype FilePrivateValue
  var filePrivateValue: FilePrivateValue { get }
}
// CHECK-LABEL: public{{(\*/)?}} class IC_PublicAssocTypeImpl : IA_PublicAssocTypeProto, IB_FilePrivateAssocTypeProto {
public class IC_PublicAssocTypeImpl: IA_PublicAssocTypeProto, IB_FilePrivateAssocTypeProto {
  public var publicValue: Int = 0
  public var filePrivateValue: Int = 0
  // CHECK-DAG: {{^}} public typealias PublicValue
  // CHECK-DAG: {{^}} public typealias FilePrivateValue
} // CHECK: {{^[}]}}

// CHECK-LABEL: private{{(\*/)?}} class ID_PrivateAssocTypeImpl : IA_PublicAssocTypeProto, IB_FilePrivateAssocTypeProto {
private class ID_PrivateAssocTypeImpl: IA_PublicAssocTypeProto, IB_FilePrivateAssocTypeProto {
  public var publicValue: Int = 0
  public var filePrivateValue: Int = 0
  // CHECK-DAG: {{^}} fileprivate typealias PublicValue
  // CHECK-DAG: {{^}} fileprivate typealias FilePrivateValue
} // CHECK: {{^[}]}}

// CHECK-LABEL: class MultipleAttributes {
class MultipleAttributes {
  // CHECK: {{^}} final {{(/\*)?private(\*/)?}} func foo()
  final private func foo() {}
} // CHECK: {{^[}]}}

// CHECK-LABEL: public{{(\*/)?}} class PublicInitBase {
public class PublicInitBase {
  // CHECK: {{^}} {{(/\*)?public(\*/)?}} init()
  public init() {}
  // CHECK: {{^}} {{(/\*)?fileprivate(\*/)?}} init(other: PublicInitBase)
  fileprivate init(other: PublicInitBase) {}
} // CHECK: {{^[}]}}

// CHECK-LABEL: public{{(\*/)?}} class PublicInitInheritor : PublicInitBase {
public class PublicInitInheritor : PublicInitBase {
  // CHECK: {{^}} public init()
  // CHECK: {{^}} fileprivate init(other: PublicInitBase)
} // CHECK: {{^[}]}}

// CHECK-LABEL: {{(/\*)?private(\*/)?}} class PublicInitPrivateInheritor : PublicInitBase {
private class PublicInitPrivateInheritor : PublicInitBase {
  // CHECK: {{^}} internal init()
  // CHECK: {{^}} fileprivate init(other: PublicInitBase)
} // CHECK: {{^[}]}}

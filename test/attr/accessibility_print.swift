// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swift-ide-test -skip-deinit=false -print-ast-typechecked -print-accessibility -source-filename=%s | FileCheck %s -check-prefix=CHECK -check-prefix=CHECK-SRC
// RUN: %target-swift-frontend -emit-module-path %t/accessibility_print.swiftmodule %s
// RUN: %target-swift-ide-test -skip-deinit=false -print-module -print-accessibility -module-to-print=accessibility_print -I %t -source-filename=%s | FileCheck %s

// This file uses alphabetic prefixes on its declarations because swift-ide-test
// sorts decls in a module before printing them.

// CHECK-LABEL: /*internal*/ var AA_defaultGlobal
var AA_defaultGlobal = 0

// CHECK: private{{(\*/)?}} var AB_privateGlobal
// CHECK: internal{{(\*/)?}} var AC_internalGlobal
// CHECK: public{{(\*/)?}} var AD_publicGlobal
private var AB_privateGlobal = 0
internal var AC_internalGlobal = 0
public var AD_publicGlobal = 0


// CHECK-LABEL: /*internal*/ struct BA_DefaultStruct {
struct BA_DefaultStruct {
  // CHECK: /*internal*/ let x
  let x = 0
} // CHECK: {{^[}]}}

// CHECK-LABEL: private{{(\*/)?}} struct BB_PrivateStruct {
private struct BB_PrivateStruct {
  // CHECK: /*private*/ var x
  var x = 0
  // CHECK: /*private*/ init(x: Int)
  // CHECK: /*private*/ init()
} // CHECK: {{^[}]}}

// CHECK-LABEL: internal{{(\*/)?}} struct BC_InternalStruct {
internal struct BC_InternalStruct {
  // CHECK: /*internal*/ let x
  let x = 0
  // CHECK: /*internal*/ init()
} // CHECK: {{^[}]}}

// CHECK-LABEL: public{{(\*/)?}} struct BD_PublicStruct {
public struct BD_PublicStruct {
  // CHECK: /*internal*/ var x
  var x = 0
  // CHECK: /*internal*/ init(x: Int)
  // CHECK: /*internal*/ init()
} // CHECK: {{^[}]}}

// CHECK-LABEL: public{{(\*/)?}} struct BE_PublicStructPrivateMembers {
public struct BE_PublicStructPrivateMembers {
  // CHECK: private{{(\*/)?}} var x
  private var x = 0
  // CHECK: /*private*/ init(x: Int)
  // CHECK: /*internal*/ init()
} // CHECK: {{^[}]}}


// CHECK-LABEL: private{{(\*/)?}} class CA_PrivateClass
private class CA_PrivateClass {
  // CHECK: {{^}} deinit
  deinit {}
  // CHECK: /*private*/ init()
} // CHECK: {{^[}]}}

// CHECK-LABEL: internal{{(\*/)?}} class CB_InternalClass
internal class CB_InternalClass {
  // CHECK: {{^}} deinit
  deinit {}
  // CHECK: /*internal*/ init()
} // CHECK: {{^[}]}}

// CHECK-LABEL: public{{(\*/)?}} class CC_PublicClass
public class CC_PublicClass {
  // CHECK: {{^}} deinit
  deinit {}
  // CHECK: /*internal*/ init()
} // CHECK: {{^[}]}}


// CHECK-LABEL: private{{(\*/)?}} enum DA_PrivateEnum {
private enum DA_PrivateEnum {
  // CHECK: {{^}} case Foo
  // CHECK: Bar
  case Foo, Bar
  // CHECK: /*private*/ init()
  init() { self = .Foo }
  // CHECK: /*private*/ var hashValue
} // CHECK: {{^[}]}}

// CHECK-LABEL: internal{{(\*/)?}} enum DB_InternalEnum {
internal enum DB_InternalEnum {
  // CHECK: {{^}} case Foo
  // CHECK: Bar
  case Foo, Bar
  // CHECK: /*internal*/ init()
  init() { self = .Foo }
  // CHECK: /*internal*/ var hashValue
} // CHECK: {{^[}]}}

// CHECK-LABEL: public{{(\*/)?}} enum DC_PublicEnum {
public enum DC_PublicEnum {
  // CHECK: {{^}} case Foo
  // CHECK: Bar
  case Foo, Bar
  // CHECK: /*internal*/ init()
  init() { self = .Foo }
  // CHECK: /*public*/ var hashValue
} // CHECK: {{^[}]}}


// CHECK-LABEL: private{{(\*/)?}} protocol EA_PrivateProtocol {
private protocol EA_PrivateProtocol {
  // CHECK: {{^}} typealias Foo
  typealias Foo
  // CHECK: /*private*/ var Bar
  var Bar: Int { get }
  // CHECK: /*private*/ func baz()
  func baz()
} // CHECK: {{^[}]}}

// CHECK-LABEL: public{{(\*/)?}} protocol EB_PublicProtocol {
public protocol EB_PublicProtocol {
  // CHECK: {{^}} typealias Foo
  typealias Foo
  // CHECK: /*public*/ var Bar
  var Bar: Int { get }
  // CHECK: /*public*/ func baz()
  func baz()
} // CHECK: {{^[}]}}


private class FA_PrivateClass {}
internal class FB_InternalClass {}
public class FC_PublicClass {}
// CHECK-SRC: {{^}}ex
// CHECK-LABEL: tension FA_PrivateClass {
extension FA_PrivateClass {
  // CHECK: /*private*/ func a()
  func a() {}
} // CHECK: {{^[}]}}

// CHECK-LABEL: extension FB_InternalClass {
extension FB_InternalClass {
  // CHECK: /*internal*/ func a()
  func a() {}
} // CHECK: {{^[}]}}

// CHECK-LABEL: extension FC_PublicClass {
extension FC_PublicClass {
  // CHECK: /*internal*/ func a()
  func a() {}
} // CHECK: {{^[}]}}


private class FD_PrivateClass {}
// CHECK-SRC: private
// CHECK-LABEL: extension FD_PrivateClass {
private extension FD_PrivateClass {
  // CHECK: /*private*/ func explicitPrivateExt()
  func explicitPrivateExt() {}
} // CHECK: {{^[}]}}


public class FE_PublicClass {}
// CHECK-SRC: private
// CHECK-LABEL: extension FE_PublicClass {
private extension FE_PublicClass {
  // CHECK: /*private*/ func explicitPrivateExt()
  func explicitPrivateExt() {}
  // CHECK: /*private*/ struct PrivateNested {
  struct PrivateNested {
    // CHECK: /*private*/ var x
    var x: Int
  } // CHECK: }
} // CHECK: {{^[}]}}

// CHECK-SRC: internal
// CHECK-LABEL: extension FE_PublicClass {
internal extension FE_PublicClass {
  // CHECK: /*internal*/ func explicitInternalExt()
  func explicitInternalExt() {}
  // CHECK: /*internal*/ struct InternalNested {
  struct InternalNested {
    // CHECK: /*internal*/ var x
    var x: Int
  } // CHECK: }
} // CHECK: {{^[}]}}

// CHECK-SRC: public
// CHECK-LABEL: extension FE_PublicClass {
public extension FE_PublicClass {
  // CHECK: /*public*/ func explicitPublicExt()
  func explicitPublicExt() {}
  // CHECK: /*public*/ struct PublicNested {
  struct PublicNested {
    // CHECK: /*internal*/ var x
    var x: Int
  } // CHECK: }
} // CHECK: {{^[}]}}


// CHECK-LABEL: /*internal*/ func GA_localTypes()
func GA_localTypes() {
  // CHECK-SRC: /*private*/ struct Local {
  struct Local {
    // CHECK-SRC: /*private*/ let x
    let x = 0
  }
  let _ = Local()

  // CHECK-SRC: /*private*/ enum LocalEnum {
  enum LocalEnum {
    // CHECK-SRC: {{^}} case A
    case A, B
  }
  let enumVal = LocalEnum.A
  _ = (enumVal == .B)
} // CHECK-SRC: {{^[}]}}

// CHECK-LABEL: public{{(\*/)?}} struct GB_NestedOuter {
public struct GB_NestedOuter {
  // CHECK: /*internal*/ struct Inner {
  struct Inner {
    // CHECK: private{{(\*/)?}} let x
    private let x = 0
    // CHECK: /*internal*/ let y
    let y = 0
  }
} // CHECK: {{^[}]}}

// CHECK-LABEL: private{{(\*/)?}} struct GC_NestedOuterPrivate {
private struct GC_NestedOuterPrivate {
  // CHECK: /*private*/ struct Inner {
  struct Inner {
    // CHECK: private{{(\*/)?}} let x
    private let x = 0
    // CHECK: /*private*/ let y
    let y = 0
  }
} // CHECK: {{^[}]}}

// CHECK-LABEL: class MultipleAttributes {
class MultipleAttributes {
  // CHECK: {{^}} final {{(/\*)?private(\*/)?}} func foo()
  final private func foo() {}
}

// CHECK-LABEL: public{{(\*/)?}} class PublicInitBase {
public class PublicInitBase {
  // CHECK: {{^}} {{(/\*)?public(\*/)?}} init()
  public init() {}
}

// CHECK-LABEL: public{{(\*/)?}} class PublicInitInheritor : PublicInitBase {
public class PublicInitInheritor : PublicInitBase {
  // CHECK: {{^}} /*public*/ init()
}

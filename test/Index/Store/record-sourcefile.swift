// RUN: rm -rf %t
// RUN: %target-swift-frontend -index-store-path %t/idx1 -o %t/file.o -typecheck %s
// RUN: %target-swift-frontend -index-store-path %t/idx2 -o %t/file.o -typecheck -primary-file %s
// RUN: c-index-test core -print-record %t/idx1 | %FileCheck %s
// RUN: c-index-test core -print-record %t/idx2 | %FileCheck %s

// CHECK: record-sourcefile.swift
// CHECK: ------------
// CHECK: struct/Swift | S1 | s:4file2S1V | <no-cgname> | Def,Ref,RelCont -
// CHECK: instance-method/acc-get/Swift | getter:property | s:4file2S1V8propertySivg | <no-cgname> | Def,Ref,Call,Impl,RelChild,RelRec,RelCall,RelAcc,RelCont - 
// CHECK: instance-property/Swift | property | [[property_USR:s:4file2S1V8propertySivp]] | <no-cgname> | Def,Ref,Read,RelChild,RelCont -
// CHECK: static-method/acc-get/Swift | getter:staticProperty | s:4file2S1V14staticPropertySivg | <no-cgname> | Def,Ref,Call,Impl,RelChild,RelRec,RelCall,RelAcc,RelCont -
// CHECK: static-property/Swift | staticProperty | s:{{.*}} | <no-cgname> | Def,Ref,Read,RelChild,RelCont -
// CHECK: instance-property/Swift | computedPropertyGetSet | s:{{.*}} | <no-cgname> | Def,RelChild -
// CHECK: struct/Swift | Int | s:Si | <no-cgname> | Ref -
// CHECK: instance-method/acc-get/Swift | getter:computedPropertyGetSet | s:4file2S1V22computedPropertyGetSetSivg | <no-cgname> | Def,RelChild,RelAcc -
// CHECK: instance-method/acc-set/Swift | setter:computedPropertyGetSet | s:4file2S1V22computedPropertyGetSetSivs | <no-cgname> | Def,RelChild,RelAcc -
// CHECK: instance-property/Swift | computedPropertyWillDid | s:{{.*}} | <no-cgname> | Def,RelChild -
// CHECK: instance-method/acc-willset/Swift | willSet:computedPropertyWillDid | s:4file2S1V23computedPropertyWillDidSivw | <no-cgname> | Def,RelChild,RelAcc -
// CHECK: instance-method/acc-didset/Swift | didSet:computedPropertyWillDid | s:4file2S1V23computedPropertyWillDidSivW | <no-cgname> | Def,RelChild,RelAcc -
// CHECK: instance-property/Swift | computedPropertyAddressor | s:{{.*}} | <no-cgname> | Def,RelChild -
// CHECK: instance-method/acc-addr/Swift | <no-name> | s:{{.*}} | <no-cgname> | Def,RelChild,RelAcc -
// CHECK: instance-method/acc-mutaddr/Swift | <no-name> | s:{{.*}} | <no-cgname> | Def,RelChild,RelAcc -
// CHECK: instance-method/Swift | method() | s:{{.*}} | <no-cgname> | Def,RelChild -
// CHECK: static-method/Swift | staticMethod() | s:{{.*}} | <no-cgname> | Def,RelChild -
// CHECK: instance-property/subscript/Swift | subscript(_:) | s:{{.*}} | <no-cgname> | Def,RelChild -
// CHECK: instance-method/acc-get/Swift | getter:subscript(_:) | s:{{.*}} | <no-cgname> | Def,RelChild,RelAcc -
// CHECK: protocol/Swift | P1 | s:{{.*}} | <no-cgname> | Def -
// CHECK: type-alias/associated-type/Swift | AT | s:{{.*}} | <no-cgname> | Def,Ref,RelChild -
// CHECK: type-alias/Swift | TA | s:{{.*}} | <no-cgname> | Def,RelChild -
// CHECK: class/Swift | C1 | s:{{.*}} | <no-cgname> | Def,Ref,RelBase,RelCont -
// CHECK: instance-method/Swift | method() | s:{{.*}} | <no-cgname> | Def,Ref,Call,Dyn,RelChild,RelRec,RelCall,RelCont -
// CHECK: class/Swift | C2 | s:{{.*}} | <no-cgname> | Def -
// CHECK: instance-method/Swift | method() | s:{{.*}} | <no-cgname> | Def,Dyn,RelChild,RelOver -
// CHECK: function/Swift | takeC1(x:) | s:{{.*}} | <no-cgname> | Def -
// CHECK: instance-method(test)/Swift | testFoo() | s:{{.*}} | <no-cgname> | Def,Dyn,RelChild -
// CHECK: ------------

// CHECK: [[@LINE+1]]:8 | struct/Swift | [[S1_USR:s:.*]] | Def | rel: 0
struct S1 {
// CHECK: [[@LINE+2]]:7 | instance-property/Swift | [[property_USR]] | Def,RelChild | rel: 1
// CHECK-NEXT:  RelChild | [[S1_USR]]
  let property = 1
// CHECK: [[@LINE+2]]:14 | static-property/Swift | [[staticProperty_USR:s:.*]] | Def,RelChild | rel: 1
// CHECK-NEXT: RelChild | [[S1_USR]]
  static let staticProperty = 2

// CHECK: [[@LINE+3]]:7 | instance-property/Swift | [[computedPropertyGetSet_USR:s:.*]] | Def,RelChild | rel: 1
// CHECK-NEXT: RelChild | [[S1_USR]]
// CHECK: [[@LINE+1]]:31 | struct/Swift | s:Si | Ref | rel: 0
  var computedPropertyGetSet: Int {
// CHECK: [[@LINE+2]]:5 | instance-method/acc-get/Swift | s:{{.*}} | Def,RelChild,RelAcc | rel: 1
// CHECK-NEXT: RelChild,RelAcc | [[computedPropertyGetSet_USR]]
    get { return 1 }
// CHECK: [[@LINE+2]]:5 | instance-method/acc-set/Swift | s:{{.*}} | Def,RelChild,RelAcc | rel: 1
// CHECK-NEXT: RelChild,RelAcc | [[computedPropertyGetSet_USR]]
    set { }
  }

// CHECK: [[@LINE+2]]:7 | instance-property/Swift | [[computedPropertyWillDid_USR:s:.*]] | Def,RelChild | rel: 1
// CHECK-NEXT: RelChild | [[S1_USR]]
  var computedPropertyWillDid: Int {
// CHECK: [[@LINE+2]]:5 | instance-method/acc-willset/Swift | s:{{.*}} | Def,RelChild,RelAcc | rel: 1
// CHECK-NEXT: RelChild,RelAcc | [[computedPropertyWillDid_USR]]
    willSet { }
// CHECK: [[@LINE+2]]:5 | instance-method/acc-didset/Swift | s:{{.*}} | Def,RelChild,RelAcc | rel: 1
// CHECK-NEXT: RelChild,RelAcc | [[computedPropertyWillDid_USR]]
    didSet { }
  }

// CHECK: [[@LINE+2]]:7 | instance-property/Swift | [[computedPropertyAddressor_USR:s:.*]] | Def,RelChild | rel: 1
// CHECK-NEXT: RelChild | [[S1_USR]]
  var computedPropertyAddressor: Int {
// CHECK: [[@LINE+2]]:5 | instance-method/acc-addr/Swift | s:{{.*}} | Def,RelChild,RelAcc | rel: 1
// CHECK-NEXT: RelChild,RelAcc | [[computedPropertyAddressor_USR]]
    unsafeAddress { }
// CHECK: [[@LINE+2]]:5 | instance-method/acc-mutaddr/Swift | s:{{.*}} | Def,RelChild,RelAcc | rel: 1
// CHECK-NEXT: RelChild,RelAcc | [[computedPropertyAddressor_USR]]
    unsafeMutableAddress { }
  }

// CHECK: [[@LINE+2]]:8 | instance-method/Swift | [[method_USR:s:.*]] | Def,RelChild | rel: 1
// CHECK-NEXT: RelChild | [[S1_USR]]
  func method() {
    _ = self
// CHECK: [[@LINE+4]]:9 | instance-method/acc-get/Swift | s:{{.*}} | Ref,Call,Impl,RelRec,RelCall,RelCont | rel: 2
// CHECK-NEXT: RelCall,RelCont | [[method_USR]]
// CHECK-NEXT: RelRec | [[S1_USR]]
// CHECK: [[@LINE+1]]:9 | instance-property/Swift | s:{{.*}} | Ref,Read,RelCont | rel: 1
    _ = property
  }

// CHECK: [[@LINE+2]]:15 | static-method/Swift | [[staticMethod_USR:s:.*]] | Def,RelChild | rel: 1
// CHECK-NEXT: RelChild | [[S1_USR]]
  static func staticMethod() {
// CHECK: [[@LINE+5]]:9 | struct/Swift | s:{{.*}} | Ref,RelCont | rel: 1
// CHECK: [[@LINE+4]]:12 | static-method/acc-get/Swift | s:{{.*}} | Ref,Call,Impl,RelRec,RelCall,RelCont | rel: 2
// CHECK-NEXT: RelCall,RelCont | [[staticMethod_USR]]
// CHECK-NEXT: RelRec | [[S1_USR]]
// CHECK: [[@LINE+1]]:12 | static-property/Swift | s:{{.*}} | Ref,Read,RelCont | rel: 1
    _ = S1.staticProperty
  }

// CHECK: [[@LINE+4]]:3 | instance-property/subscript/Swift | [[S1_subscript_USR:s:.*]] | Def,RelChild | rel: 1
// CHECK-NEXT: RelChild | [[S1_USR]]
// CHECK: [[@LINE+2]]:28 | instance-method/acc-get/Swift | s:{{.*}} | Def,RelChild,RelAcc | rel: 1
// CHECK-NEXT: RelChild,RelAcc | [[S1_subscript_USR]]
  subscript(x: Int) -> Int { return 1 }
}

// CHECK: [[@LINE+1]]:10 | protocol/Swift | [[P1_USR:s:.*]] | Def | rel: 0
protocol P1 {
// CHECK: [[@LINE+2]]:18 | type-alias/associated-type/Swift | s:{{.*}} | Def,RelChild | rel: 1
// CHECK-NEXT: RelChild | [[P1_USR]]
  associatedtype AT
// CHECK: [[@LINE+3]]:13 | type-alias/Swift | s:{{.*}} | Def,RelChild | rel: 1
// CHECK-NEXT: RelChild | [[P1_USR]]
// CHECK: [[@LINE+1]]:18 | type-alias/associated-type/Swift | s:{{.*}} | Ref | rel: 0
  typealias TA = AT
}

// CHECK: [[@LINE+1]]:7 | class/Swift | [[C1_USR:s:.*]] | Def | rel: 0
class C1 {
// CHECK: [[@LINE+2]]:8 | instance-method/Swift | [[C1_foo_USR:s:.*]] | Def,Dyn,RelChild | rel: 1
// CHECK-NEXT: RelChild | [[C1_USR]]
  func method() {}
}
// CHECK: [[@LINE+3]]:7 | class/Swift | [[C2_USR:s:.*]] | Def | rel: 0
// CHECK: [[@LINE+2]]:12 | class/Swift | [[C1_USR]] | Ref,RelBase | rel: 1
// CHECK-NEXT: RelBase | [[C2_USR]]
class C2 : C1 {
// CHECK: [[@LINE+3]]:17 | instance-method/Swift | s:{{.*}} | Def,Dyn,RelChild,RelOver | rel: 2
// CHECK-NEXT: RelOver | [[C1_foo_USR]]
// CHECK-NEXT: RelChild | [[C2_USR]]
  override func method() {}
}

// CHECK: [[@LINE+2]]:6 | function/Swift | [[takeC1_USR:s:.*]] | Def | rel: 0
// CHECK: [[@LINE+1]]:16 | class/Swift | s:{{.*}} | Ref,RelCont | rel: 1
func takeC1(x: C1) {
// CHECK: [[@LINE+3]]:5 | instance-method/Swift | s:{{.*}} | Ref,Call,Dyn,RelRec,RelCall,RelCont | rel: 2
// CHECK-NEXT: RelCall,RelCont | [[takeC1_USR]]
// CHECK-NEXT: RelRec | [[C1_USR]]
  x.method()
}

func test1() {}
class XCTestCase {}
class MyTestCase: XCTestCase {
// CHECK: [[@LINE+2]]:8 | instance-method(test)/Swift | s:{{.*}} | Def,Dyn,RelChild | rel: 1
// CHECK-NEXT: RelChild | s:4file10MyTestCaseC
  func testFoo() { test1() }
}

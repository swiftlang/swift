// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s -emit-sil | FileCheck %s

import gizmo
import objc_protocols_Bas

protocol [class_protocol, objc] NSRuncing {
  func runce() -> NSObject
  func copyRuncing() -> NSObject

  func foo()
}

protocol [class_protocol, objc] NSFunging {
  func funge()

  func foo()
}

protocol Ansible {
  func anse()
}

// CHECK: sil @_T14objc_protocols12objc_genericUSo9NSRuncing__FT1xQ__TCSo8NSObjectS1__
func objc_generic<T:NSRuncing>(x:T) -> (NSObject, NSObject) {
  return (x.runce(), x.copyRuncing())
  // -- Result of runce is retain_autoreleased according to default objc conv
  // CHECK: [[METHOD:%.*]] = archetype_method [volatile] {{\$.*}},  #NSRuncing.runce!1.objc
  // CHECK: [[RESULT1:%.*]] = apply [[METHOD]]([[THIS1:%.*]]) : $[cc(objc_method), thin] (T, ()) -> NSObject
  // CHECK: retain_autoreleased [[RESULT1]] : $NSObject

  // -- Result of copyRuncing is received retained according to -copy family
  // CHECK: [[METHOD:%.*]] = archetype_method [volatile] {{\$.*}},  #NSRuncing.copyRuncing!1.objc
  // CHECK: [[RESULT2:%.*]] = apply [[METHOD]]([[THIS2:%.*]]) : $[cc(objc_method), thin] (T, ()) -> NSObject
  // CHECK-NOT: retain_autoreleased

  // -- Arguments are not consumed by objc calls
  // CHECK: release [[THIS2]]
  // CHECK: release [[THIS1]]
}

// CHECK: sil @_T14objc_protocols13objc_protocolFT1xPSo9NSRuncing__TCSo8NSObjectS1__
func objc_protocol(x:NSRuncing) -> (NSObject, NSObject) {
  return (x.runce(), x.copyRuncing())
  // -- Result of runce is retain_autoreleased according to default objc conv
  // CHECK: [[THIS1:%.*]] = project_existential_ref [[THIS1_ORIG:%.*]] :
  // CHECK: [[METHOD:%.*]] = protocol_method [volatile] [[THIS1_ORIG]] : {{.*}}, #NSRuncing.runce!1.objc
  // CHECK: [[RESULT1:%.*]] = apply [[METHOD]]([[THIS1]]) : $[cc(objc_method), thin] (Builtin.ObjCPointer, ()) -> NSObject
  // CHECK: retain_autoreleased [[RESULT1]] : $NSObject

  // -- Result of copyRuncing is received retained according to -copy family
  // CHECK: [[THIS2:%.*]] = project_existential_ref [[THIS2_ORIG:%.*]] :
  // CHECK: [[METHOD:%.*]] = protocol_method [volatile] [[THIS2_ORIG]] : {{.*}}, #NSRuncing.copyRuncing!1.objc
  // CHECK: [[RESULT2:%.*]] = apply [[METHOD]]([[THIS2:%.*]]) : $[cc(objc_method), thin] (Builtin.ObjCPointer, ()) -> NSObject
  // CHECK-NOT: retain_autoreleased

  // -- Arguments are not consumed by objc calls
  // CHECK: release [[THIS2_ORIG]]
  // CHECK: release [[THIS1_ORIG]]
}

// CHECK: sil @_T14objc_protocols25objc_protocol_compositionFT1xPSo9NSRuncingSo9NSFunging__T_
func objc_protocol_composition(x:protocol<NSRuncing, NSFunging>) {
  // CHECK: [[THIS:%.*]] = project_existential_ref [[THIS_ORIG:%.*]] : $protocol<NSFunging, NSRuncing>
  // CHECK: [[METHOD:%.*]] = protocol_method [volatile] [[THIS_ORIG]] : {{.*}}, #NSRuncing.runce!1.objc
  // CHECK: apply [[METHOD]]([[THIS]]) : $[cc(objc_method), thin] (Builtin.ObjCPointer, ()) -> NSObject
  x.runce()

  // CHECK: [[THIS:%.*]] = project_existential_ref [[THIS_ORIG:%.*]] : $protocol<NSFunging, NSRuncing>
  // CHECK: [[METHOD:%.*]] = protocol_method [volatile] [[THIS_ORIG]] : {{.*}}, #NSFunging.funge!1.objc
  // CHECK: apply [[METHOD]]([[THIS]]) : $[cc(objc_method), thin] (Builtin.ObjCPointer, ()) -> ()
  x.funge()
}
// -- ObjC thunks get emitted for ObjC protocol conformances

class Foo : NSRuncing, NSFunging, Ansible {
  // -- NSRuncing
  func runce() -> NSObject { return NSObject() }
  func copyRuncing() -> NSObject { return NSObject() }

  // -- NSFunging
  func funge() {}

  // -- Both NSRuncing and NSFunging
  func foo() {}

  // -- Ansible
  func anse() {}
}

// CHECK: sil @_TToC14objc_protocols3Foo5runcefS0_FT_CSo8NSObject
// CHECK: sil @_TToC14objc_protocols3Foo11copyRuncingfS0_FT_CSo8NSObject
// CHECK: sil @_TToC14objc_protocols3Foo5fungefS0_FT_T_
// CHECK: sil @_TToC14objc_protocols3Foo3foofS0_FT_T_
// CHECK-NOT: sil @_TTo{{.*}}anse{{.*}}

class Bar { }

extension Bar : NSRuncing {
  func runce() -> NSObject { return NSObject() }
  func copyRuncing() -> NSObject { return NSObject() }
  func foo() {}
}

// CHECK: sil @_TToC14objc_protocols3Bar5runcefS0_FT_CSo8NSObject
// CHECK: sil @_TToC14objc_protocols3Bar11copyRuncingfS0_FT_CSo8NSObject
// CHECK: sil @_TToC14objc_protocols3Bar3foofS0_FT_T_

// class Bas from objc_protocols_Bas module
extension Bas : NSRuncing {
  // runce() implementation from the original definition of Bas
  func copyRuncing() -> NSObject { return NSObject() }
  func foo() {}
}

// CHECK: sil @_TToC18objc_protocols_Bas3Bas11copyRuncingfS0_FT_CSo8NSObject
// CHECK: sil @_TToC18objc_protocols_Bas3Bas3foofS0_FT_T_
// CHECK: sil @_TToC18objc_protocols_Bas3Bas5runcefS0_FT_CSo8NSObject

// -- Inherited objc protocols

protocol Fungible : NSFunging { }

class Zim : Fungible {
  func funge() {}
  func foo() {}
}

// CHECK: sil @_TToC14objc_protocols3Zim5fungefS0_FT_T_
// CHECK: sil @_TToC14objc_protocols3Zim3foofS0_FT_T_

// class Zang from objc_protocols_Bas module
extension Zang : Fungible {
  // funge() implementation from the original definition of Zim
  func foo() {}
}

// CHECK: sil @_TToC18objc_protocols_Bas4Zang3foofS0_FT_T_
// CHECK: sil @_TToC18objc_protocols_Bas4Zang5fungefS0_FT_T_

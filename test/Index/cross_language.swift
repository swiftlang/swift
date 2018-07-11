// REQUIRES: objc_interop

// RUN: mkdir -p %t
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -module-name cross_language -import-objc-header %S/Inputs/cross_language_bridge_head.h -D SWIFT_CODE -print-indexed-symbols -source-filename %s > %t.idx.out
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %s -module-name cross_language -import-objc-header %S/Inputs/cross_language_bridge_head.h -D SWIFT_CODE -emit-objc-header-path %t/objc_header.h

// This concatenates the swift and objc file into one so we can CHECK them together. This ensures that the USRs are in-sync between swift and clang.

// RUN: cat %s > %t/combined.m
// RUN: cat %S/Inputs/cross_language.m >> %t/combined.m
// RUN: c-index-test core -print-source-symbols -- %t/combined.m -F %clang-importer-sdk-path/frameworks -I %t -isysroot %S/../Inputs/clang-importer-sdk >> %t.idx.out
// RUN: %FileCheck %t/combined.m -input-file %t.idx.out

#if SWIFT_CODE

import Foundation

@objc public class MyCls1 : NSObject {
// CHECK: [[@LINE-1]]:20 | class/Swift | MyCls1 | [[MyCls1_USR:.*]] | Def
  @objc public func someMeth() {}
  // CHECK: [[@LINE-1]]:21 | instance-method/Swift | someMeth() | [[MyCls1_someMeth_USR:.*]] | Def
  // CHECK: [[@LINE-4]]:38 | constructor/Swift | init() | [[MyCls1_init_USR:.*]] | Def,Impl,RelChild,RelOver | rel: 2
  // CHECK-NEXT: RelOver | constructor/Swift | init() | c:objc(cs)NSObject(im)init
  // CHECK-NEXT: RelChild | class/Swift | MyCls1 | [[MyCls1_USR]]
}

@objc public class MyCls2 : NSObject {
  @objc public init(withInt: Int) {}
  // CHECK: [[@LINE-1]]:16 | constructor/Swift | init(withInt:) | [[MyCls2_initwithInt_USR:.*]] | Def
}

@objc public protocol MyProt {
// CHECK: [[@LINE-1]]:23 | protocol/Swift | MyProt | [[MyProt_USR:.*]] | Def
  func someProtMeth()
  // CHECK: [[@LINE-1]]:8 | instance-method/Swift | someProtMeth() | [[MyProt_someProtMeth_USR:.*]] | Def
}

public extension MyCls1 {
// CHECK: [[@LINE-1]]:18 | extension/ext-class/Swift | MyCls1 | s:e:c:@CM@cross_language@objc(cs)MyCls1(im)someExtMeth |
// CHECK: [[@LINE-2]]:18 | class/Swift | MyCls1 | [[MyCls1_USR]] |
  @objc public func someExtMeth() {}
  // CHECK: [[@LINE-1]]:21 | instance-method/Swift | someExtMeth() | [[MyCls1_someExtMeth_USR:.*]] | Def
}

public extension SomeObjCClass {
  // CHECK: [[@LINE-1]]:18 | class/Swift | SomeObjCClass | [[SomeObjCClass_USR:.*]] | Ref
  @objc public func someSwiftExtMeth() {}
  // CHECK: [[@LINE-1]]:21 | instance-method/Swift | someSwiftExtMeth() | [[SomeObjCClass_someSwiftExtMeth_USR:.*]] | Def
}

@objc public enum MyEnum : Int {
// CHECK: [[@LINE-1]]:19 | enum/Swift | MyEnum | [[MyEnum_USR:.*]] | Def
  case someEnumConst = 1
  // CHECK: [[@LINE-1]]:8 | enumerator/Swift | someEnumConst | [[MyEnum_someEnumConst_USR:.*]] | Def
}

#endif

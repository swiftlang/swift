// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/mods)
// RUN: split-file %s %t

// RUN: %target-swift-ide-test -print-indexed-symbols -enable-objc-interop -source-filename %t/ObjcUser.swift -Xcc -fmodule-map-file=%t/module.modulemap | %FileCheck -dump-input=always %t/ObjcUser.swift

//--- objc_decls.h
#import <Foundation/Foundation.h>
@protocol MemberAdding<NSObject>
@property int protocolAddedProperty;
-(void)protocolAddedMethod;
@end

@interface ObjCClass : NSObject
@property int baseClassProperty;
@end

@interface ObjCClass (category) <MemberAdding>
@property int categoryAddedProperty;
@end

extern NSErrorDomain const TestErrorDomain;

typedef NS_ERROR_ENUM(TestErrorDomain, TestError){
    TestErrorFoo = 0,
    TestErrorBar = 1,
};

typedef NS_ENUM(NSInteger, ErrCategory) {
  ErrCategoryA = 0,
  ErrCategoryB = 1,
};

//--- module.modulemap
module objc_decls {
  header "objc_decls.h"
  export *
}

//--- ObjcUser.swift
import objc_decls
func test() { // CHECK: [[@LINE]]:6 | function/Swift | test() | [[s:.*]] | Def | rel: 0
  let c = ObjCClass()
  let _ = c.baseClassProperty
  // CHECK: [[@LINE-1]]:13 | instance-property/Swift | baseClassProperty | c:objc(cs)ObjCClass(py)baseClassProperty | Ref,Read,Dyn,RelRec,RelCont | rel: 2
  let _ = c.categoryAddedProperty
  // CHECK: [[@LINE-1]]:13 | instance-property/Swift | categoryAddedProperty | c:objc(cs)ObjCClass(py)categoryAddedProperty | Ref,Read,Dyn,RelRec,RelCont | rel: 2
  // CHECK: [[@LINE-2]]:13 | instance-method/acc-get/Swift | getter:categoryAddedProperty | c:@CM@objc_decls@@objc(cs)ObjCClass(im)categoryAddedProperty | Ref,Call,Dyn,Impl,RelRec,RelCall,RelCont | rel: 2
  let _ = c.protocolAddedProperty
  // CHECK: [[@LINE-1]]:13 | instance-property/Swift | protocolAddedProperty | c:objc(pl)MemberAdding(py)protocolAddedProperty | Ref,Read,Dyn,RelRec,RelCont | rel: 2
  // CHECK: [[@LINE-2]]:13 | instance-method/acc-get/Swift | getter:protocolAddedProperty | c:@CM@objc_decls@@objc(cs)ObjCClass(im)protocolAddedProperty | Ref,Call,Dyn,Impl,RelRec,RelCall,RelCont | rel: 2 
  c.protocolAddedMethod()
  // CHECK: [[@LINE-1]]:5 | instance-method/Swift | protocolAddedMethod() | c:objc(pl)MemberAdding(im)protocolAddedMethod | Ref,Call,Dyn,RelRec,RelCall,RelCont | rel: 2 
}
class SubObjCClass: ObjCClass {
  override func protocolAddedMethod() {}
  // CHECK: [[@LINE-1]]:17 | instance-method/Swift | protocolAddedMethod() | c:@M@swift_ide_test@objc(cs)SubObjCClass(im)protocolAddedMethod | Def,Dyn,RelChild,RelOver | rel: 2
  // CHECK-NEXT: RelOver | instance-method/Swift | protocolAddedMethod() | c:objc(pl)MemberAdding(im)protocolAddedMethod
  // CHECK-NEXT: RelChild | class/Swift | SubObjCClass | c:@M@swift_ide_test@objc(cs)SubObjCClass
}
func testEnumIndex() {
  let _: TestError.Code = .bar
  // CHECK: [[@LINE-1]]:10 | struct/Swift | TestError | c:@E@TestError | Ref | rel: 0
  // CHECK: [[@LINE-2]]:20 | enum/Swift | Code | c:@E@TestError | Ref,RelCont | rel: 1
  // CHECK: [[@LINE-3]]:28 | enumerator/Swift | bar | c:@E@TestError@TestErrorBar | Ref,RelCont | rel: 1
  let e = TestError.foo
  // CHECK: [[@LINE-1]]:11 | struct/Swift | TestError | c:@E@TestError | Ref,RelCont | rel: 1
  // CHECK: [[@LINE-2]]:21 | static-property/Swift | foo | c:@E@TestError@TestErrorFoo | Ref,Read,RelCont | rel: 1
  switch e {
    case TestError.foo: break
    // CHECK: [[@LINE-1]]:10 | struct/Swift | TestError | c:@E@TestError | Ref,RelCont | rel: 1
    // CHECK: [[@LINE-2]]:20 | static-property/Swift | foo | c:@E@TestError@TestErrorFoo | Ref,Read,RelCont | rel: 1
    case .bar: break
    // CHECK: [[@LINE-1]]:11 | enumerator/Swift | bar | c:@E@TestError@TestErrorBar | Ref,RelCont | rel: 1
    default: break
  }
  let _ = ErrCategory.B
  // CHECK: [[@LINE-1]]:11 | enum/Swift | ErrCategory | c:@E@ErrCategory | Ref,RelCont | rel: 1
  // CHECK: [[@LINE-2]]:23 | enumerator/Swift | B | c:@E@ErrCategory@ErrCategoryB | Ref,RelCont | rel: 1
  let c: ErrCategory = .A
  // CHECK: [[@LINE-1]]:10 | enum/Swift | ErrCategory | c:@E@ErrCategory | Ref,RelCont | rel: 1
  // CHECK: [[@LINE-2]]:25 | enumerator/Swift | A | c:@E@ErrCategory@ErrCategoryA | Ref,RelCont | rel: 1
  switch c {
    case ErrCategory.A: break
    // CHECK: [[@LINE-1]]:22 | enumerator/Swift | A | c:@E@ErrCategory@ErrCategoryA | Ref,RelCont | rel: 1
    // CHECK: [[@LINE-2]]:10 | enum/Swift | ErrCategory | c:@E@ErrCategory | Ref,RelCont | rel: 1
    // CHECK: [[@LINE-3]]:22 | enum/Swift | ErrCategory | c:@E@ErrCategory | Ref,RelCont | rel: 1
    case .B: break
    // CHECK: [[@LINE-1]]:11 | enumerator/Swift | B | c:@E@ErrCategory@ErrCategoryB | Ref,RelCont | rel: 1
  }
}

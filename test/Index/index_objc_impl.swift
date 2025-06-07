// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/mods)
// RUN: split-file %s %t

// We use `-index-store-path` to exercise a code path that used to crash; we
// aren't checking its output.
// RUN: %target-swift-frontend -emit-module -o %t/mods %t/ObjcImpl.swift -import-objc-header %t/objc_impl.h -target %target-stable-abi-triple -disable-objc-attr-requires-foundation-module -index-store-path %t/index-store
// RUN: %target-swift-ide-test -print-indexed-symbols -module-to-print ObjcImpl -source-filename none -I %t/mods -target %target-stable-abi-triple | %FileCheck %s

//--- objc_impl.h
@interface NSObject
- (instancetype)init;
@end

@interface ObjCClass : NSObject
@property int someObjCDeclaredVar;
@end

//--- ObjcImpl.swift
// CHECK: extension/ext-class/Swift | ObjCClass | s:e:c:@CM@ObjcImpl@@objc(cs)ObjCClass(py)someObjCDeclaredVar | Def
// CHECK: class/Swift | ObjCClass | c:objc(cs)ObjCClass | Ref
@_objcImplementation public extension ObjCClass {
  // CHECK: instance-property/Swift | someObjCDeclaredVar | c:@CM@ObjcImpl@@objc(cs)ObjCClass(py)someObjCDeclaredVar | Def
  @objc var someObjCDeclaredVar: CInt = 1

  // Implicitly synthesized `override init()`:
  // CHECK: constructor/Swift | init() | c:@CM@ObjcImpl@@objc(cs)ObjCClass(im)init | Def
}

// REQUIRES: swift_swift_parser, objc_interop

// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/objc_cxx_macro_definitions.swift -g -no-toolchain-stdlib-rpath
// RUN: %target-build-swift -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) %s -o %t/main -module-name MacroUser -swift-version 5
// RUN: %target-codesign %t/main

// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk) -typecheck -swift-version 5 -parse-as-library -enable-objc-interop -emit-objc-header-path %t/MacroUser.h -load-plugin-library %t/%target-library-name(MacroDefinition) %s -module-name MacroUser
// RUN: %FileCheck --check-prefix CHECK %s < %t/MacroUser.h

@attached(member, names: prefixed(member_))
public macro ObjCMemberFunc() = #externalMacro(module: "MacroDefinition", type: "ObjCMemberFuncMacro")

@attached(peer, names: prefixed(peer_))
public macro ObjCPeerFunc() = #externalMacro(module: "MacroDefinition", type: "ObjCPeerFuncMacro")

@freestanding(declaration, names: named(member_freestanding))
public macro ObjCFreestandingFunc() = #externalMacro(module: "MacroDefinition", type: "ObjCFreestandingFuncMacro")

@freestanding(declaration, names: named(MacroExpandedObjCClass))
public macro ObjCFreestandingClass() = #externalMacro(module: "MacroDefinition", type: "ObjCFreestandingClassMacro")

@attached(extension, conformances: MyObjCProtocol, names: named(objcRequirement))
public macro ObjCExtension() = #externalMacro(module: "MacroDefinition", type: "ObjCExtensionMacro")

@objc public protocol MyObjCProtocol {
  func objcRequirement()
}

// ---

import Foundation

// CHECK:      SWIFT_CLASS("_TtC9MacroUser1A")
// CHECK-NEXT: @interface A : NSObject
@ObjCMemberFunc
public class A: NSObject {
  @ObjCPeerFunc
  @objc public func a() {}
  // CHECK-DAG: - (void)a;
  // CHECK-DAG: - (void)member_A;
  // CHECK-DAG: - (void)peer_a;

  #ObjCFreestandingFunc
  // CHECK-DAG: - (void)member_freestanding;
}
// CHECK-DAG: @end


// CHECK:      SWIFT_CLASS("_TtC9MacroUser1B")
// CHECK-NEXT: @interface B : NSObject

// CHECK:      @interface B (SWIFT_EXTENSION(MacroUser)) <MyObjCProtocol>
// CHECK-NEXT: - (void)objcRequirement;
// CHECK-NEXT: @end
@ObjCExtension
public class B: NSObject {}


// CHECK:      SWIFT_CLASS("_TtC9MacroUser22MacroExpandedObjCClass")
// CHECK-NEXT: @interface MacroExpandedObjCClass : NSObject
// CHECK-DAG:  - (void)member;
// CHECK-DAG:  @end
#ObjCFreestandingClass

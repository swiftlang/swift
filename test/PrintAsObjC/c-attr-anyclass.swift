/// AnyClass is representable in C, printed as the Objective-C runtime type
/// 'Class'. A @c function using it stays in the plain-C section (not the
/// #if __OBJC__ block), and the C section includes <objc/objc.h> so that
/// 'Class' is defined even for a plain C consumer of the generated header.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -emit-module \
// RUN:   -verify -o %t -emit-objc-header-path %t/anyclass.h \
// RUN:   -disable-objc-attr-requires-foundation-module
// RUN: %FileCheck %s --input-file %t/anyclass.h

// REQUIRES: objc_interop

// CHECK: #include <objc/objc.h>
// CHECK: extern "C" {

import Foundation

@c(cFunc)
func cFunc() { }
// CHECK: void cFunc(void)

@c(returnsAnyClass)
func returnsAnyClass() -> AnyClass { fatalError() }
// CHECK: Class _Nonnull returnsAnyClass(void)

@c(takesAnyClass)
func takesAnyClass(_ cls: AnyClass) { }
// CHECK: void takesAnyClass(Class _Nonnull cls)

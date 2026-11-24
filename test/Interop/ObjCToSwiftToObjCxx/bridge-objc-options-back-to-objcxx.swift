// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/use-objc-options.swift -module-name UseObjCOptions -typecheck -verify -emit-clang-header-path %t/UseObjCOptions.h -I %t -cxx-interoperability-mode=default -clang-header-expose-decls=all-public

// RUN: %FileCheck %s < %t/UseObjCOptions.h

// RUN: echo "#include \"header.h\"" > %t/full-header.h
// RUN: cat %t/UseObjCOptions.h >> %t/full-header.h

// RUN: %target-interop-build-clangxx -std=gnu++20 -fobjc-arc -c -x objective-c++-header %t/full-header.h -o %t/o.o

// REQUIRES: objc_interop

//--- header.h
#import <Foundation/Foundation.h>

typedef NS_OPTIONS(NSUInteger, ObjCOptions) {
  ObjCOptionsNone = 0,
  ObjCOptionsFirst = 1 << 0,
};

typedef NS_ENUM(NSInteger, ObjCEnum) {
  ObjCEnumFirst,
  ObjCEnumSecond,
};

//--- module.modulemap
module ObjCOptionsModule {
  header "header.h"
}

//--- use-objc-options.swift
import ObjCOptionsModule

public func takesOptionsHandler(_ handler: @escaping (ObjCOptions) -> Void) {}

public func takesEnumHandler(_ handler: @escaping (ObjCEnum) -> Void) {}

// In C++, NS_OPTIONS declares an anonymous enum whose name is a typedef of the
// underlying integer type, so the generated header can't specialize generic
// traits for it.

// CHECK-NOT: isUsableInGenericContext<>
// CHECK-NOT: TypeMetadataTrait<>
// CHECK: isUsableInGenericContext<ObjCEnum>
// CHECK: TypeMetadataTrait<ObjCEnum>
// CHECK-NOT: isUsableInGenericContext<>
// CHECK-NOT: TypeMetadataTrait<>

// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %S/../Inputs/empty.swift -typecheck -emit-objc-header-path %t/empty.h
// RUN: %clang -E -fobjc-arc -fmodules -isysroot %clang-importer-sdk-path -I %t %s | %FileCheck %s

// REQUIRES: objc_interop

#import "empty.h"

@class ABC; // CHECK-LABEL: @class ABC;
SWIFT_CLASS(abc, "my_module") // CHECK-NEXT: __attribute__((external_source_symbol(language="Swift", defined_in="my_module", generated_declaration))) __attribute__((objc_runtime_name(abc)))
@interface ABC // CHECK-NEXT: @interface
@end

@class DEF; // CHECK-LABEL: @class DEF;
SWIFT_CLASS_NAMED(def, "other") // CHECK-NEXT: __attribute__((external_source_symbol(language="Swift", defined_in="other", generated_declaration))) __attribute__((swift_name(def)))
@interface DEF // CHECK-NEXT: @interface
@end

@protocol AAA; // CHECK-LABEL: @protocol AAA;
SWIFT_PROTOCOL(aaa, "module_foo") // CHECK-NEXT: __attribute__((external_source_symbol(language="Swift", defined_in="module_foo", generated_declaration))) __attribute__((objc_runtime_name(aaa)))
@protocol AAA // CHECK-NEXT: @protocol
@end

@protocol BBB; // CHECK-LABEL: @protocol BBB;
SWIFT_PROTOCOL_NAMED(bbb, "my_module") // CHECK-NEXT: __attribute__((external_source_symbol(language="Swift", defined_in="my_module", generated_declaration))) __attribute__((swift_name(bbb)))
@protocol BBB // CHECK-NEXT: @protocol
@end

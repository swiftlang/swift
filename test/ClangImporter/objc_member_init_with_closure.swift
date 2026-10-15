// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -emit-silgen %t/Theme.swift -import-objc-header %t/Theme.h | %FileCheck %s

//--- Theme.h
#import <Foundation.h>

typedef struct ThemeFuncTable {
  NSString *_Nonnull (*_Nonnull keyGetter)();
  NSArray *_Nonnull (*_Nonnull arrayGetter)();
  NSDictionary *_Nonnull (*_Nonnull dictGetter)();
  NSSet *_Nonnull (*_Nonnull setGetter)();
} ThemeFuncTable;

typedef NSString *_Nonnull (^StringReturningCallback)(void);
void doSomethingString(StringReturningCallback keyGetter);

//--- Theme.swift
import Theme

// Verify that Swift closures have the correct bridged types, and that
// bridging thunks reference the correct closure by name.

let _ = ThemeFuncTable(

    // CHECK: sil private [ossa] @[[STRING_CLOSURE:\$[^ ]+]] : $@convention(thin) () -> @owned String {
    // CHECK: } // end sil function

    // CHECK-LABEL: sil private [thunk] [ossa] @{{.*}}To : $@convention(c) () -> @autoreleased NSString {
    // CHECK:   [[SWIFT_FN:%.*]] = function_ref @[[STRING_CLOSURE]] : $@convention(thin) () -> @owned String
    // CHECK:   [[RESULT:%.*]] = apply [[SWIFT_FN]]()
    // CHECK:   [[BRIDGE_FN:%.*]] = function_ref @$sSS10FoundationE19_bridgeToObjectiveCSo8NSStringCyF
    // CHECK:   apply [[BRIDGE_FN]]
    // CHECK: } // end sil function
    keyGetter: { "SomeTheme" },

    // CHECK: sil private [ossa] @[[ARRAY_CLOSURE:\$[^ ]+]] : $@convention(thin) () -> @owned Array<Any> {
    // CHECK: } // end sil function

    // CHECK-LABEL: sil private [thunk] [ossa] @{{.*}}To : $@convention(c) () -> @autoreleased NSArray {
    // CHECK:   [[SWIFT_FN:%.*]] = function_ref @[[ARRAY_CLOSURE]] : $@convention(thin) () -> @owned Array<Any>
    // CHECK:   [[RESULT:%.*]] = apply [[SWIFT_FN]]()
    // CHECK:   [[BRIDGE_FN:%.*]] = function_ref @$sSa10FoundationE19_bridgeToObjectiveCSo7NSArrayCyF
    // CHECK:   apply [[BRIDGE_FN]]
    // CHECK: } // end sil function
    arrayGetter: { [1, 2, 3] },

    // CHECK: sil private [ossa] @[[DICT_CLOSURE:\$[^ ]+]] : $@convention(thin) () -> @owned Dictionary<AnyHashable, Any> {
    // CHECK: } // end sil function

    // CHECK-LABEL: sil private [thunk] [ossa] @{{.*}}To : $@convention(c) () -> @autoreleased NSDictionary {
    // CHECK:   [[SWIFT_FN:%.*]] = function_ref @[[DICT_CLOSURE]] : $@convention(thin) () -> @owned Dictionary<AnyHashable, Any>
    // CHECK:   [[RESULT:%.*]] = apply [[SWIFT_FN]]()
    // CHECK:   [[BRIDGE_FN:%.*]] = function_ref @$sSD10FoundationE19_bridgeToObjectiveCSo12NSDictionaryCyF
    // CHECK:   apply [[BRIDGE_FN]]
    // CHECK: } // end sil function
    dictGetter: { ["key": "value"] },

    // CHECK: sil private [ossa] @[[SET_CLOSURE:\$[^ ]+]] : $@convention(thin) () -> @owned Set<AnyHashable> {
    // CHECK: } // end sil function

    // CHECK-LABEL: sil private [thunk] [ossa] @{{.*}}To : $@convention(c) () -> @autoreleased NSSet {
    // CHECK:   [[SWIFT_FN:%.*]] = function_ref @[[SET_CLOSURE]] : $@convention(thin) () -> @owned Set<AnyHashable>
    // CHECK:   [[RESULT:%.*]] = apply [[SWIFT_FN]]()
    // CHECK:   [[BRIDGE_FN:%.*]] = function_ref @$sSh10FoundationE19_bridgeToObjectiveCSo5NSSetCyF
    // CHECK:   apply [[BRIDGE_FN]]
    // CHECK: } // end sil function
    setGetter: { Set(["a", "b"]) }
)

// Verify the memberwise init takes @convention(c) function pointers and
// assembles them into the struct.

// CHECK-LABEL: sil shared [transparent] [serialized] [ossa] @{{.*}}ThemeFuncTable{{.*}} : $@convention(method) (@convention(c) () -> @autoreleased NSString, @convention(c) () -> @autoreleased NSArray, @convention(c) () -> @autoreleased NSDictionary, @convention(c) () -> @autoreleased NSSet, @thin ThemeFuncTable.Type) -> ThemeFuncTable {
// CHECK:   [[RESULT:%.*]] = struct $ThemeFuncTable
// CHECK:   return [[RESULT]]
// CHECK: } // end sil function

doSomethingString({ "SomeTheme" })

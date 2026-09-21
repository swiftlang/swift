// RUN: %empty-directory(%t/src)
// RUN: %empty-directory(%t/sdk)
// RUN: split-file %s %t/src

// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) %t/src/main.swift \
// RUN:   -enable-objc-interop \
// RUN:   -import-objc-header %t/src/Test.h \
// RUN:   -swift-version 5 \
// RUN:   -module-name main -I %t -verify -verify-ignore-unrelated | %FileCheck %t/src/main.swift

// REQUIRES: objc_interop

//--- Test.h
@import Foundation;

#define MAIN_ACTOR __attribute__((__swift_attr__("@MainActor")))

#pragma clang assume_nonnull begin

@interface Doc : NSObject
- (void)saveWithCompletionHandler:(void (^ __nullable)(BOOL success))completionHandler MAIN_ACTOR;
- (void)undoWithCompletionHandler:(void (^ __nullable)(BOOL success))completionHandler;
@end

#pragma clang assume_nonnull end

//--- main.swift

// CHECK-LABEL: sil hidden [ossa] @$s4main4test3docySo3DocC_tYaF : $@convention(thin) @async (@guaranteed Doc) -> () {
// CHECK: [[GENERIC_EXECUTOR:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
// CHECK: hop_to_executor [[GENERIC_EXECUTOR]]
// CHECK: [[METHOD_REF:%.*]] = objc_method %0, #Doc.save!foreign
// CHECK: [[MAIN_ACTOR_GETTER:%.*]] = function_ref @$sScM6sharedScMvgZ : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// CHECK: [[MAIN_ACTOR_REF:%.*]] = apply [[MAIN_ACTOR_GETTER]]({{.*}}) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// CHECK: [[MAIN_ACTOR:%.*]] = begin_borrow [[MAIN_ACTOR_REF]]
// CHECK: hop_to_executor [[MAIN_ACTOR]]
// CHECK: apply [[METHOD_REF]]({{.*}}, %0)
// CHECK: await_async_continuation {{.*}}, resume bb1
// CHECK: bb1:
// CHECK: hop_to_executor [[GENERIC_EXECUTOR]]
// CHECK: } // end sil function '$s4main4test3docySo3DocC_tYaF'
func test(doc: Doc) async {
  _ = await doc.save()
}

// CHECK-LABEL: sil hidden [ossa] @$s4main17testSameIsolation3docySo3DocC_tYaF : $@convention(thin) @async (@guaranteed Doc) -> () {
// CHECK: [[MAIN_ACTOR_GETTER:%.*]] = function_ref @$sScM6sharedScMvgZ : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// CHECK: [[MAIN_ACTOR_REF:%.*]] = apply [[MAIN_ACTOR_GETTER]]({{.*}}) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// CHECK: [[MAIN_ACTOR:%.*]] = begin_borrow [[MAIN_ACTOR_REF]]
// CHECK: hop_to_executor [[MAIN_ACTOR]]
// CHECK: [[METHOD_REF:%.*]] = objc_method %0, #Doc.save!foreign
// CHECK: apply [[METHOD_REF]]({{.*}}, %0)
// CHECK-NOT: hop_to_executor {{.*}}
// CHECK: await_async_continuation {{.*}}, resume bb1
// CHECK: bb1:
// CHECK: hop_to_executor [[MAIN_ACTOR]]
// CHECK: } // end sil function '$s4main17testSameIsolation3docySo3DocC_tYaF'
@MainActor
func testSameIsolation(doc: Doc) async {
  _ = await doc.save()
}

// CHECK-LABEL: sil hidden [ossa] @$s4main12testNoSwitch3docySo3DocC_tYaF : $@convention(thin) @async (@guaranteed Doc) -> () {
// CHECK: [[MAIN_ACTOR_GETTER:%.*]] = function_ref @$sScM6sharedScMvgZ : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// CHECK: [[MAIN_ACTOR_REF:%.*]] = apply [[MAIN_ACTOR_GETTER]]({{.*}}) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// CHECK: [[MAIN_ACTOR:%.*]] = begin_borrow [[MAIN_ACTOR_REF]]
// CHECK: hop_to_executor [[MAIN_ACTOR]]
// CHECK: [[METHOD_REF:%.*]] = objc_method %0, #Doc.undo!foreign
// CHECK: apply [[METHOD_REF]]({{.*}}, %0)
// CHECK-NOT: hop_to_executor {{.*}}
// CHECK: await_async_continuation {{.*}}, resume bb1
// CHECK: bb1:
// CHECK: hop_to_executor [[MAIN_ACTOR]]
// CHECK: } // end sil function '$s4main12testNoSwitch3docySo3DocC_tYaF'
@MainActor
func testNoSwitch(doc: Doc) async {
  _ = await doc.undo() // `undo` is `nonisolated(nonsending)`
}

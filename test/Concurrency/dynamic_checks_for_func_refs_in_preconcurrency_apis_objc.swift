// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -target arm64-apple-macosx10.15 -enable-upcoming-feature DynamicActorIsolation -Xllvm -sil-print-types -emit-silgen -verify %s | %FileCheck %s
// REQUIRES: objc_interop
// REQUIRES: swift_feature_DynamicActorIsolation

import Foundation

install_global_event_handler { @MainActor _ in
  // expected-warning@-1 {{converting function value of type '@MainActor (Any) -> ()' to 'event_handler' (aka '@convention(c) (Any) -> ()') loses global actor 'MainActor'; this is an error in the Swift 6 language mode}}
}

// CHECK-LABEL: sil private [thunk] [ossa] @$s56dynamic_checks_for_func_refs_in_preconcurrency_apis_objcyypScMYccfU_To : $@convention(c) (AnyObject) -> ()
// CHECK: [[MAIN_ACTOR_METATYPE:%.*]] = metatype $@thick MainActor.Type
// CHECK: [[SHARED_FIELD_GETTER:%.*]] = function_ref @$sScM6sharedScMvgZ : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// CHECK-NEXT: [[MAIN_ACTOR:%.*]] = apply [[SHARED_FIELD_GETTER]]([[MAIN_ACTOR_METATYPE]]) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// CHECK-NEXT: [[MAIN_ACTOR_ACCESS:%.*]] = begin_borrow [[MAIN_ACTOR]] : $MainActor
// CHECK-NEXT: [[EXEC:%.*]] = extract_executor [[MAIN_ACTOR_ACCESS]] : $MainActor
// CHECK: [[CHECK_EXEC_REF:%.*]] = function_ref @$ss22_checkExpectedExecutor14_filenameStart01_D6Length01_D7IsASCII5_line9_executoryBp_BwBi1_BwBetF
// CHECK-NEXT: {{.*}} = apply [[CHECK_EXEC_REF]]({{.*}}, [[EXEC]])

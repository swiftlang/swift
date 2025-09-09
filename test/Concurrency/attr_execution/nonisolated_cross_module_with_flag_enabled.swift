// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

/// Build the library A
// RUN: %target-swift-frontend -emit-module %t/src/A.swift \
// RUN:   -module-name A -swift-version 6 -enable-library-evolution \
// RUN:   -enable-upcoming-feature NonisolatedNonsendingByDefault \
// RUN:   -emit-module-path %t/A.swiftmodule \
// RUN:   -emit-module-interface-path %t/A.swiftinterface

// RUN: %target-swift-typecheck-module-from-interface(%t/A.swiftinterface) -module-name A

// Build the client using module
// RUN: %target-swift-emit-sil -verify -module-name Client -I %t %t/src/Client.swift | %FileCheck %t/src/Client.swift

// RUN: rm %t/A.swiftmodule

// Re-build the client using interface
// RUN: %target-swift-emit-sil -verify -module-name Client -I %t %t/src/Client.swift | %FileCheck %t/src/Client.swift

//--- A.swift
@MainActor

public final class Test {
  public nonisolated func test() async {}
}

//--- Client.swift
import A

// CHECK-LABEL: sil hidden @$s6Client4test1ty1A4TestC_tYaF : $@convention(thin) @async (@guaranteed Test) -> ()
// CHECK: bb0([[SELF:%.*]] : $Test):
// CHECK:  [[MAIN_ACTOR_EXISTENTIAL:%.*]] = init_existential_ref %4 : $MainActor : $MainActor, $any Actor
// CHECK:  [[ANY_ACTOR:%.*]] = enum $Optional<any Actor>, #Optional.some!enumelt, [[MAIN_ACTOR_EXISTENTIAL]]
// CHECK:  [[TEST_METHOD:%.*]] = function_ref @$s1A4TestC4testyyYaF : $@convention(method) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed Test) -> ()
// CHECK:  apply [[TEST_METHOD]]([[ANY_ACTOR]], [[SELF]])
// CHECK: } // end sil function '$s6Client4test1ty1A4TestC_tYaF'
@MainActor
func test(t: Test) async {
  await t.test() // Ok
}

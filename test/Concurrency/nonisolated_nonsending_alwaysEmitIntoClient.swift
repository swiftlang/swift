// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

/// Build the library A
// RUN: %target-swift-frontend -emit-module %t/src/A.swift \
// RUN:   -module-name A -swift-version 6 -enable-library-evolution \
// RUN:   -emit-module-path %t/A.swiftmodule \
// RUN:   -emit-module-interface-path %t/A.swiftinterface

// Build the client using module
// RUN: %target-swift-emit-sil -verify -module-name Client -I %t %t/src/Client.swift | %FileCheck %t/src/Client.swift

// RUN: rm %t/A.swiftmodule

// Re-build the client using interface
// RUN: %target-swift-emit-sil -verify -module-name Client -I %t %t/src/Client.swift | %FileCheck %t/src/Client.swift

//--- A.swift

@usableFromInline
func computeAnswer() async throws -> Int {
  42
}

@_alwaysEmitIntoClient
public nonisolated(nonsending) func computeUltimateAnswer() async throws -> Int {
  try await computeAnswer()
}

//--- Client.swift
import A

// CHECK: // computeUltimateAnswer()
// CHECK-NEXT: // Isolation: nonisolated(nonsending)
// CHECK-LABEL sil shared [export_implementation] @$s1A21computeUltimateAnswerSiyYaF : $@convention(thin) @caller_isolated @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> Int
// CHECK: bb0([[ISOLATION:%.*]] : $Builtin.ImplicitActor):
// CHECK:   // function_ref computeAnswer()
// CHECK:   [[COMPUTE:%.*]] = function_ref @$s1A13computeAnswerSiyYaKF : $@convention(thin) @async () -> (Int, @error any Error)
// CHECK:   try_apply [[COMPUTE]]() : $@convention(thin) @async () -> (Int, @error any Error), normal bb2, error bb1
// CHECK: bb1(%3 : $any Error):
// CHECK:   hop_to_executor [[ISOLATION]]
// CHECK:   throw {{.*}}
// CHECK: bb2({{.*}} : $Int):
// CHECK:   hop_to_executor [[ISOLATION]]
// CHECK:   return {{.*}}
// CHECK: } // end sil function '$s1A21computeUltimateAnswerSiyYaKF'

func test() async {
  let _ = try? await computeUltimateAnswer()
}

/// Round trip test for SIL with module aliasing

// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

/// Create module Tea
// RUN: %target-swift-frontend -module-name Tea %t/FileTea.swift -emit-module -emit-module-path %t/Tea.swiftmodule

/// Emit SIL with module aliasing
// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-sil %t/FileBar.swift  -module-alias Coffee=Tea -I %t -o %t/Bar-output1.sil

/// Verify the module real name 'Tea' is contained in the generated SIL
// RUN: %FileCheck %s -input-file %t/Bar-output1.sil

/// Reingest the SIL file and verify it contains the same result
// RUN: %target-sil-opt -sil-print-types -enable-sil-verify-all %t/Bar-output1.sil -I %t -o %t/Bar-output2.sil
// RUN: %FileCheck %s -input-file %t/Bar-output2.sil

// CHECK: sil_stage canonical

// CHECK: import Builtin
// CHECK: import Swift
// CHECK: import SwiftShims

// CHECK: import Tea

// CHECK: public func drink() -> Mild?

// CHECK: // drink()
// CHECK: sil @$s4main5drink3Tea4MildVSgyF : $@convention(thin) () -> Optional<Mild> {
// CHECK: bb0:
// CHECK: // function_ref brew()
// CHECK:   %0 = function_ref @$s3Tea4brewAA4MildVSgyF : $@convention(thin) () -> Optional<Mild> // user: %1
// CHECK:   %1 = apply %0() : $@convention(thin) () -> Optional<Mild> // user: %2
// CHECK:   return %1 : $Optional<Mild>                     // id: %2
// CHECK: } // end sil function '$s4main5drink3Tea4MildVSgyF'

// CHECK: // brew()
// CHECK: sil [noinline] @$s3Tea4brewAA4MildVSgyF : $@convention(thin) () -> Optional<Mild>



// BEGIN FileTea.swift
public struct Mild {
  public init() {}
}

@inline(never)
public func brew() -> Tea.Mild? {
  return Mild()
}

// BEGIN FileBar.swift
import Coffee

public func drink() -> Mild? {
  return brew()
}


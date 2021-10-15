// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -module-name Tea %t/FileTea.swift -emit-module -emit-module-path %t/Tea.swiftmodule

// RUN: %target-swift-frontend -emit-sil %t/FileBar.swift  -module-alias Coffee=Tea -I %t -o %t/Bar-output.sil
// RUN: %FileCheck %s -input-file %t/Bar-output.sil

// RUN: %target-sil-opt -enable-sil-verify-all %t/Bar-import-real.sil -I %t -o %t/Bar-re-output.sil
// RUN: %FileCheck %s -input-file %t/Bar-re-output.sil -check-prefix RE-CHECK

// CHECK: sil_stage canonical

// CHECK: import Builtin
// CHECK: import Swift
// CHECK: import SwiftShims

// CHECK: import Coffee

// CHECK: public func drink() -> Mild?

// CHECK: // main
// CHECK: sil @main : $@convention(c) (Int32, UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int8>>>) -> Int32 {
// CHECK: bb0(%0 : $Int32, %1 : $UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int8>>>):
// CHECK:   %2 = integer_literal $Builtin.Int32, 0          // user: %3
// CHECK:   %3 = struct $Int32 (%2 : $Builtin.Int32)        // user: %4
// CHECK:   return %3 : $Int32                              // id: %4
// CHECK: } // end sil function 'main'

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

// RE-CHECK: sil_stage canonical

// RE-CHECK: import Builtin
// RE-CHECK: import Swift
// RE-CHECK: import SwiftShims

// RE-CHECK: import Tea

// RE-CHECK: public func drink() -> Mild?

// RE-CHECK: // main
// RE-CHECK: sil @main : $@convention(c) (Int32, UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int8>>>) -> Int32 {
// RE-CHECK: bb0(%0 : $Int32, %1 : $UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int8>>>):
// RE-CHECK:   %2 = integer_literal $Builtin.Int32, 0          // user: %3
// RE-CHECK:   %3 = struct $Int32 (%2 : $Builtin.Int32)        // user: %4
// RE-CHECK:   return %3 : $Int32                              // id: %4
// RE-CHECK: } // end sil function 'main'

// RE-CHECK: // drink()
// RE-CHECK: sil @$s4main5drink3Tea4MildVSgyF : $@convention(thin) () -> Optional<Mild> {
// RE-CHECK: bb0:
// RE-CHECK: // function_ref brew()
// RE-CHECK:   %0 = function_ref @$s3Tea4brewAA4MildVSgyF : $@convention(thin) () -> Optional<Mild> // user: %1
// RE-CHECK:   %1 = apply %0() : $@convention(thin) () -> Optional<Mild> // user: %2
// RE-CHECK:   return %1 : $Optional<Mild>                     // id: %2
// RE-CHECK: } // end sil function '$s4main5drink3Tea4MildVSgyF'

// RE-CHECK: // brew()
// RE-CHECK: sil [noinline] @$s3Tea4brewAA4MildVSgyF : $@convention(thin) () -> Optional<Mild>

// BEGIN Bar-import-real.sil
sil_stage canonical

import Builtin
import Swift
import SwiftShims

import Tea

public func drink() -> Mild?

// main
sil @main : $@convention(c) (Int32, UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int8>>>) -> Int32 {
bb0(%0 : $Int32, %1 : $UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int8>>>):
  %2 = integer_literal $Builtin.Int32, 0          // user: %3
  %3 = struct $Int32 (%2 : $Builtin.Int32)        // user: %4
  return %3 : $Int32                              // id: %4
} // end sil function 'main'

// drink()
sil @$s4main5drink3Tea4MildVSgyF : $@convention(thin) () -> Optional<Mild> {
bb0:
  // function_ref brew()
  %0 = function_ref @$s3Tea4brewAA4MildVSgyF : $@convention(thin) () -> Optional<Mild> // user: %1
  %1 = apply %0() : $@convention(thin) () -> Optional<Mild> // user: %2
  return %1 : $Optional<Mild>                     // id: %2
} // end sil function '$s4main5drink3Tea4MildVSgyF'

// brew()
sil [noinline] @$s3Tea4brewAA4MildVSgyF : $@convention(thin) () -> Optional<Mild>


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


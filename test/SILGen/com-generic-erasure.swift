// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -module-name Library -enable-library-evolution -emit-sil -sil-verify-all %S/../Inputs/COMGenericArguments.swift %S/../Inputs/COMGenericErasure.swift | %FileCheck %s --check-prefix=CHECK
// RUN: %target-swift-frontend -enable-experimental-com-interop -enable-sil-opaque-values -I %t -module-name Library -enable-library-evolution -emit-sil -sil-verify-all %S/../Inputs/COMGenericArguments.swift %S/../Inputs/COMGenericErasure.swift | %FileCheck %s --check-prefix=CHECK
// RUN: %target-swift-frontend -enable-experimental-com-interop -enable-sil-opaque-values -I %t -module-name Library -enable-library-evolution -emit-silgen -sil-verify-all %S/../Inputs/COMGenericArguments.swift %S/../Inputs/COMGenericErasure.swift | %FileCheck %s --check-prefix=RAW

// Borrowing projection directly uses the incoming generic storage.
// CHECK-LABEL: sil [noinline] @$s7Library5erase
// CHECK-SAME: (@in_guaranteed T) -> @owned any IItem
// CHECK-NOT: alloc_stack
// CHECK: [[RESULT:%.*]] = init_com_existential %0 : $*T : $T, $any IItem
// CHECK-NOT: destroy_addr %0
// CHECK: return [[RESULT]]

// Consuming the source still requires its cleanup after projection.
// CHECK-LABEL: sil [noinline] @$s7Library13eraseConsumed
// CHECK-SAME: (@in T) -> @owned any IItem
// CHECK: [[RESULT:%.*]] = init_com_existential {{%.*}} : $*T : $T, $any IItem
// CHECK: destroy_addr
// CHECK: return [[RESULT]]

// CHECK-LABEL: sil [noinline] @$s7Library9inherited
// CHECK-SAME: <T where T : IExtended>
// CHECK: init_com_existential {{%.*}} : $*T : $T, $any IItem

// AnyObject does not change generic COM storage into a Swift class reference.
// CHECK-LABEL: sil [noinline] @$s7Library10eraseClass
// CHECK-SAME: (@in_guaranteed T) -> @owned any IClassItem
// CHECK: init_com_existential {{%.*}} : $*T : $T, $any IClassItem

// Refinement forwards the opened interface's ownership and address point.
// CHECK-LABEL: sil [noinline] @$s7Library6refine
// CHECK: [[OPEN:%.*]] = open_com_existential
// CHECK: [[RESULT:%.*]] = init_existential_ref [[OPEN]] : $@opened({{.*}}, any IExtended) Self
// CHECK: return [[RESULT]]

// An explicit copy owns separate storage, which the projection borrows.
// CHECK-LABEL: sil [noinline] @$s7Library12copyBorrowed
// CHECK-SAME: (@in_guaranteed T) -> @owned any IItem
// CHECK: [[TEMP:%.*]] = alloc_stack $T
// CHECK: copy_addr %0 to [init] [[TEMP]]
// CHECK-NOT: alloc_stack
// CHECK: [[RESULT:%.*]] = init_com_existential [[TEMP]] : $*T : $T, $any IItem
// CHECK-NEXT: destroy_addr [[TEMP]]
// CHECK: return [[RESULT]]

// Projection does not consume an explicitly borrowed parameter.
// CHECK-LABEL: sil [noinline] @$s7Library13eraseBorrowed
// CHECK-SAME: (@in_guaranteed T) -> @owned any IItem
// CHECK-NOT: alloc_stack
// CHECK: [[RESULT:%.*]] = init_com_existential %0 : $*T : $T, $any IItem
// CHECK-NOT: destroy_addr %0
// CHECK: return [[RESULT]]

// RAW-LABEL: sil [noinline] [ossa] [opaque] @$s7Library5erase
// RAW: bb0([[SOURCE:%.*]] : @guaranteed $T):
// RAW-NOT: alloc_stack
// RAW-NOT: copy_value
// RAW: [[RESULT:%.*]] = init_com_existential [[SOURCE]] : $T : $T, $any IItem
// RAW-NOT: destroy_value [[SOURCE]]
// RAW: return [[RESULT]]

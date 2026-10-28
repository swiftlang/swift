// Verifies how a `@cxx @implementation` function taking a `const` reference to
// a non-trivial or move-only C++ class as a `borrowing` parameter is lowered:
// the parameter keeps the C++ convention, `@in_guaranteed`, and the body reads
// the caller's object in place, without copying or destroying it.

// RUN: %target-swift-emit-silgen \
// RUN:   -module-name main \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -I %S/Inputs \
// RUN:   %s | %FileCheck %s

// REQUIRES: swift_feature_CxxImplementation

import NonTrivial

// int readTracked(const Tracked &t);
// CHECK-LABEL: sil [asmname "{{.*}}readTracked{{.*}}"] [ossa] @$s4main11readTrackedys5Int32VSo0C0VFTo : $@convention(c) (@in_guaranteed Tracked) -> Int32 {
// CHECK:       bb0(%0 : @noImplicitCopy $*Tracked):
// CHECK-NOT:     {{copy_addr|load \[copy\]|load \[take\]|destroy_addr}}
// CHECK:         struct_element_addr {{%.*}}, #Tracked.value
// CHECK-NOT:     {{copy_addr|load \[copy\]|load \[take\]|destroy_addr}}
// CHECK:       } // end sil function '$s4main11readTrackedys5Int32VSo0C0VFTo'
@unsafe @cxx @implementation
public func readTracked(_ t: borrowing Tracked) -> Int32 { return t.value }

// int readMoveOnly(const MoveOnly &m);
// CHECK-LABEL: sil [asmname "{{.*}}readMoveOnly{{.*}}"] [ossa] @$s4main12readMoveOnlyys5Int32VSo0cD0VFTo : $@convention(c) (@in_guaranteed MoveOnly) -> Int32 {
// CHECK:       bb0(%0 : $*MoveOnly):
// CHECK:         mark_unresolved_non_copyable_value [no_consume_or_assign] %0
// CHECK-NOT:     {{copy_addr|load \[copy\]|load \[take\]|destroy_addr}}
// CHECK:         struct_element_addr {{%.*}}, #MoveOnly.value
// CHECK-NOT:     {{copy_addr|load \[copy\]|load \[take\]|destroy_addr}}
// CHECK:       } // end sil function '$s4main12readMoveOnlyys5Int32VSo0cD0VFTo'
@unsafe @cxx @implementation
public func readMoveOnly(_ m: borrowing MoveOnly) -> Int32 { return m.value }

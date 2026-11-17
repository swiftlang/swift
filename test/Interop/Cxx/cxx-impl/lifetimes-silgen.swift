// Verifies how a `@cxx @implementation` function whose C++ declaration carries
// lifetime annotations is lowered. A lifetimebound `const T &` parameter keeps
// the C++ convention, `@in_guaranteed`, and a non-escapable result depends on
// it.

// RUN: %target-swift-emit-silgen \
// RUN:   -module-name main \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -I %S/Inputs \
// RUN:   %s | %FileCheck %s

// REQUIRES: swift_feature_CxxImplementation
// REQUIRES: swift_feature_Lifetimes

import Lifetimes

// TrackedView viewTracked(const Tracked &t [[clang::lifetimebound]]);
// CHECK-LABEL: sil [asmname "{{.*}}viewTracked{{.*}}"] [ossa] @$s4main11viewTrackedySo0C4ViewVSo0C0VFTo : $@convention(c) (@in_guaranteed Tracked) -> @lifetime(borrow {{.*}}0) @owned TrackedView {
// CHECK:       bb0(%0 : @noImplicitCopy $*Tracked):
// CHECK-NOT:     {{copy_addr|load \[copy\]|load \[take\]|destroy_addr}}
// CHECK:       } // end sil function '$s4main11viewTrackedySo0C4ViewVSo0C0VFTo'
@unsafe @cxx @implementation
@_lifetime(borrow t)
public func viewTracked(_ t: borrowing Tracked) -> TrackedView {
  return TrackedView(value: nil)
}

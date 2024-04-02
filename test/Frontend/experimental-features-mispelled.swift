// This test verifies that command line parsing catches mispelled feature names

// RUN: not %target-swift-frontend -typecheck %s \
// RUN:     -enable-experimental-feature SomeBogusFeature 2>&1 \
// RUN:     | %FileCheck %s --check-prefix CHECK-EXPERIMENTAL

// RUN: not %target-swift-ide-test -print-ast-typechecked -source-filename=%s \
// RUN:     -enable-experimental-feature SomeBogusFeature 2>&1 \
// RUN:     | %FileCheck %s --check-prefix CHECK-EXPERIMENTAL

// RUN: not %sil-opt %s \
// RUN:     -enable-experimental-feature SomeBogusFeature 2>&1 \
// RUN:     | %FileCheck %s --check-prefix CHECK-EXPERIMENTAL

/// ---------------------------------------------------------------------------

// RUN: not %target-swift-frontend -typecheck %s \
// RUN:     -enable-upcoming-feature SomeBogusUpcomingFeature 2>&1 \
// RUN:     | %FileCheck %s --check-prefix CHECK-UPCOMING

// RUN: not %target-swift-ide-test -print-ast-typechecked -source-filename=%s \
// RUN:     -enable-upcoming-feature SomeBogusUpcomingFeature 2>&1 \
// RUN:     | %FileCheck %s --check-prefix CHECK-UPCOMING

// RUN: not %sil-opt %s \
// RUN:     -enable-upcoming-feature SomeBogusUpcomingFeature 2>&1 \
// RUN:     | %FileCheck %s --check-prefix CHECK-UPCOMING

// CHECK-EXPERIMENTAL: unknown experimental feature "SomeBogusFeature"
// CHECK-UPCOMING: unknown upcoming feature "SomeBogusUpcomingFeature"

import Swift

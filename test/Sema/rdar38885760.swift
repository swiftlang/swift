// RUN: %empty-directory(%t)
//
// RUN: not %target-swift-frontend -typecheck %s -parse-stdlib 2>%t/fallback_diagnostic.txt
// RUN: %FileCheck %s --check-prefix FALLBACK-DIAGNOSTIC <%t/fallback_diagnostic.txt
//
// FALLBACK-DIAGNOSTIC: error: failed to produce diagnostic for expression; please file a bug report

import Swift

Builtin.trigger_fallback_diagnostic()

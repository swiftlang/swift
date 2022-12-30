// RUN: %empty-directory(%t)
//
// RUN: not %target-swift-frontend -typecheck %s -parse-stdlib 2>%t/fallback_diagnostic.txt
// RUN: %FileCheck %s --check-prefix FALLBACK-DIAGNOSTIC <%t/fallback_diagnostic.txt
//
// FALLBACK-DIAGNOSTIC: error: failed to produce diagnostic for expression; please submit a bug report (https://swift.org/contributing/#reporting-bugs){{$}}

import Swift

Builtin.trigger_fallback_diagnostic()

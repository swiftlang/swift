// RUN: %empty-directory(%t)
// RUN: mkdir %t/stats
// RUN: mkdir %t/cache
// RUN: env SWIFT_ACCEPT_NON_RESILIENT_INTERFACES=1 \
// RUN:   %target-swift-frontend -typecheck %s -I %S/Inputs -stats-output-dir %t/stats -module-cache-path %t/cache

import empty

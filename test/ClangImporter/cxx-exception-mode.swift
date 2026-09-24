// RUN: %target-swift-frontend -typecheck %s -cxx-exception-mode=annotated
// RUN: not %target-swift-frontend -typecheck %s -cxx-exception-mode=invalid 2>&1 | %FileCheck %s --check-prefix=INVALID
// RUN: not %target-swift-frontend -typecheck %s -cxx-exception-mode=strict 2>&1 | %FileCheck %s --check-prefix=REQUIRES-BOTH
// RUN: not %target-swift-frontend -typecheck %s -cxx-exception-mode=strict -cxx-interoperability-mode=default 2>&1 | %FileCheck %s --check-prefix=REQUIRES-FEATURE
// RUN: %target-swift-frontend -typecheck %s -cxx-exception-mode=strict -cxx-exception-mode=annotated

// INVALID: invalid value 'invalid' in '-cxx-exception-mode=invalid'
// REQUIRES-BOTH: '-cxx-exception-mode=strict' requires C++ interoperability
// REQUIRES-BOTH: '-cxx-exception-mode=strict' requires '-enable-experimental-feature CxxExceptionBridging'
// REQUIRES-FEATURE: '-cxx-exception-mode=strict' requires '-enable-experimental-feature CxxExceptionBridging'

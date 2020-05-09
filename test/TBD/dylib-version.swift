// REQUIRES: VENDOR=apple 
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-ir -o /dev/null %s -tbd-current-version 2.0.3 -tbd-compatibility-version 1.7 -emit-tbd -emit-tbd-path %t/both_provided.tbd
// RUN: %target-swift-frontend -emit-ir -o /dev/null %s -tbd-current-version 2.0 -emit-tbd -emit-tbd-path %t/only_current_provided.tbd
// RUN: %target-swift-frontend -emit-ir -o /dev/null %s -tbd-compatibility-version 2 -emit-tbd -emit-tbd-path %t/only_compat_provided.tbd
// RUN: not %target-swift-frontend -emit-ir -o /dev/null %s -tbd-compatibility-version not_a_version_string -emit-tbd -emit-tbd-path /dev/null 2>&1 | %FileCheck %s --check-prefix BOGUS

// RUN: %FileCheck %s --check-prefix BOTH < %t/both_provided.tbd
// RUN: %FileCheck %s --check-prefix CURRENT < %t/only_current_provided.tbd
// RUN: %FileCheck %s --check-prefix COMPAT < %t/only_compat_provided.tbd

// BOTH: current-version: 2.0.3
// BOTH: compatibility-version: 1.7
// CURRENT: current-version: 2
// COMPAT: compatibility-version: 2

// BOGUS: invalid dynamic library compatibility version 'not_a_version_string'

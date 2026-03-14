// REQUIRES: VENDOR=apple 
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-ir -o /dev/null %s -tbd-install_name some_dylib -tbd-current-version 2.0.3 -tbd-compatibility-version 1.7 -emit-tbd -emit-tbd-path %t/both_provided.tbd
// RUN: %target-swift-frontend -emit-ir -o /dev/null %s -tbd-install_name some_dylib -tbd-current-version 2.0 -emit-tbd -emit-tbd-path %t/only_current_provided.tbd
// RUN: %target-swift-frontend -emit-ir -o /dev/null %s -tbd-install_name some_dylib -tbd-compatibility-version 2 -emit-tbd -emit-tbd-path %t/only_compat_provided.tbd
// RUN: not %target-swift-frontend -emit-ir -o /dev/null %s -tbd-install_name some_dylib -tbd-compatibility-version not_a_version_string -emit-tbd -emit-tbd-path /dev/null 2>&1 | %FileCheck %s --check-prefix BOGUS

// RUN: %validate-json %t/both_provided.tbd | %FileCheck %s --check-prefix BOTH
// RUN: %validate-json %t/only_current_provided.tbd | %FileCheck %s --check-prefix CURRENT
// RUN: %validate-json %t/only_compat_provided.tbd | %FileCheck %s --check-prefix COMPAT

// BOTH: compatibility_versions
// BOTH: "version": "1.7"
// BOTH: current_versions
// BOTH: "version": "2.0.3"

// CURRENT: current_versions
// CURRENT: "version": "2"

// COMPAT: compatibility_versions
// COMPAT: "version": "2"

// BOGUS: invalid dynamic library compatibility version 'not_a_version_string'

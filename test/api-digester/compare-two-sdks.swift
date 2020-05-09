// REQUIRES: OS=macosx

// RUN: %empty-directory(%t.mod1)
// RUN: %empty-directory(%t.mod2)
// RUN: %empty-directory(%t.sdk)
// RUN: %empty-directory(%t.module-cache)

// RUN: %api-digester -diagnose-sdk -print-module -module-list-file %S/Inputs/mock-sdk-modules.txt -sdk %S/Inputs/mock-sdk.sdk -bsdk %S/Inputs/mock-sdk-baseline.sdk -module-cache-path %t.module-cache -o %t.result -abort-on-module-fail

// RUN: %clang -E -P -x c %S/Outputs/mock-sdk-api.txt -o - | sed '/^\s*$/d' > %t.expected
// RUN: %clang -E -P -x c %t.result -o - | sed '/^\s*$/d' > %t.result.tmp
// RUN: diff -u %t.expected %t.result.tmp

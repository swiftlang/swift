// REQUIRES: OS=macosx
// RUN: %empty-directory(%t.mod)
// RUN: %empty-directory(%t.sdk)
// RUN: %empty-directory(%t.module-cache)
// RUN: %api-digester -generate-empty-baseline -o %t.result -avoid-tool-args
// RUN: diff -u %S/Outputs/empty-baseline.json %t.result
// RUN: %api-digester -deserialize-sdk -input-paths %S/Outputs/empty-baseline.json -o %t.result
// RUN: diff -u %S/Outputs/empty-baseline.json %t.result

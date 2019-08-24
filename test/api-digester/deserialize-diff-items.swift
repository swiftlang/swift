// RUN: %api-digester -deserialize-diff --input-paths %S/Outputs/macro-gen.json -o %t.result -json
// RUN: diff -u %S/Outputs/macro-gen.json %t.result

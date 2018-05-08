// REQUIRES: objc_interop
// RUN: %empty-directory(%t) && %target-swift-frontend -c -update-code -primary-file %s -F %S/mock-sdk -api-diff-data-file %S/Inputs/API.json -emit-migrated-file-path %t/iuo_any_coercion.swift.result -emit-remap-file-path %t/iuo_any_coercion.swift.remap -o /dev/null
// RUN: diff -u %S/iuo_any_coercion.swift.expected %t/iuo_any_coercion.swift.result

let x: Int! = 1
let _: Any = x

let y: Int? = 1
let _: Any = y

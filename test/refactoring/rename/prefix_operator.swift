let foo: Int = 12
let negfoo = -foo
print("opposite of \(foo) is \(negfoo)")

// REQUIRES: swift_swift_parser
// RUN: %empty-directory(%t.result)

// RUN: %refactor -find-local-rename-ranges -source-filename %s -pos=1:5 -new-name bar >> %t.result/def.swift
// RUN: diff -u %S/Outputs/prefix_operator/refactor.swift.expected %t.result/def.swift

// RUN: %refactor -find-local-rename-ranges -source-filename %s -pos=2:15 -new-name bar >> %t.result/operator_ref.swift
// RUN: diff -u %S/Outputs/prefix_operator/refactor.swift.expected %t.result/operator_ref.swift

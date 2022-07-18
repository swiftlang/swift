let foo: Int = 12
let negfoo = -foo
print("opposite of \(foo) is \(negfoo)")

// RUN: %empty-directory(%t.result)

// RUN: %refactor -rename -source-filename %s -pos=1:5 -new-name bar >> %t.result/def.swift
// RUN: %target-swift-frontend-typecheck %t.result/def.swift
// RUN: diff -u %S/Outputs/prefix_operator/refactor.swift.expected %t.result/def.swift

// RUN: %refactor -rename -source-filename %s -pos=2:15 -new-name bar >> %t.result/operator_ref.swift
// RUN: %target-swift-frontend-typecheck %t.result/operator_ref.swift
// RUN: diff -u %S/Outputs/prefix_operator/refactor.swift.expected %t.result/operator_ref.swift

func throwingFunc() throws -> [Int] {
    return []
}
struct X { let array: [Int] }
let _ = X(array: try! throwingFunc())
// RUN: %empty-directory(%t.result)
// RUN: %refactor -convert-to-do-catch -source-filename %s -pos=5:19 > %t.result/L5.swift
// RUN: diff -u %S/Outputs/inside_constructor/L5.swift.expected %t.result/L5.swift

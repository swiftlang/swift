func throwingFunc() throws -> Int? {
    return nil
}
if let val = try! throwingFunc() { 
    let _ = val
}
// RUN: %empty-directory(%t.result)
// RUN: %refactor -convert-to-do-catch -source-filename %s -pos=4:16 > %t.result/L5.swift
// RUN: diff -u %S/Outputs/in_if_clause_with_let_binding/L5.swift.expected %t.result/L5.swift

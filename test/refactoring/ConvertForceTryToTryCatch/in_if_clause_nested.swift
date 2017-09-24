func throwingFunc() throws -> Bool {
    return true
}
if 4 > 3 {
    if try! throwingFunc() { 
        let _ = 3
    }
}
// RUN: rm -rf %t.result && mkdir -p %t.result
// RUN: %refactor -convert-to-do-catch -source-filename %s -pos=5:10 > %t.result/L5.swift
// RUN: diff -u %S/Outputs/in_if_clause_nested/L5.swift.expected %t.result/L5.swift

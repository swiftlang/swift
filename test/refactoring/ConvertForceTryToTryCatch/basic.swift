func throwingFunc() throws {
    return
}
try! throwingFunc()
// RUN: %empty-directory(%t.result)
// RUN: %refactor -convert-to-do-catch -source-filename %s -pos=4:3 > %t.result/L5.swift
// RUN: diff -u %S/Outputs/basic/L5.swift.expected %t.result/L5.swift

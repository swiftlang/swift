func throwingFunc() throws -> [Int] {
    return []
}
for num in try! throwingFunc() {
    let _ = num
}
// RUN: %empty-directory(%t.result)
// RUN: %refactor -convert-to-do-catch -source-filename %s -pos=4:14 > %t.result/L5.swift
// RUN: diff -u %S/Outputs/for_loop/L5.swift.expected %t.result/L5.swift

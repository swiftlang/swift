func throwingFunc() throws {
    return
}
func foo() {
    try! throwingFunc()
}
// RUN: rm -rf %t.result && mkdir -p %t.result
// RUN: %refactor -convert-to-do-catch -source-filename %s -pos=5:7 > %t.result/L5.swift
// RUN: diff -u %S/Outputs/basic_in_func/L5.swift.expected %t.result/L5.swift

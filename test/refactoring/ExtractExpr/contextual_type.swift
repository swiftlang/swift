func foo(_: Int32) {}
func bar() {
  foo(1+1)  
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor -extract-expr -source-filename %s -pos=3:7 -end-pos=3:10 >> %t.result/C7-10.swift
// RUN: diff -u %S/Outputs/contextual_type/C7-10.swift.expected %t.result/C7-10.swift

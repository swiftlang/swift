func foo() {
  let a = 3
  if a > 2 {
    if a < 10 {}
  }
}
// RUN: rm -rf %t.result && mkdir -p %t.result
// RUN: %refactor -collapse-nested-if -source-filename %s -pos=3:3 > %t.result/L3-3.swift
// RUN: diff -u %S/Outputs/basic/L3-3.swift.expected %t.result/L3-3.swift

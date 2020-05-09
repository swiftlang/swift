func foo() {
  _ = 1234567
  _ = 1234567.12345
  _ = +1234567
  _ = -1234567
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor -simplify-long-number -source-filename %s -pos=2:9 > %t.result/Integer.swift
// RUN: diff -u %S/Outputs/Integer.expected %t.result/Integer.swift
// RUN: %refactor -simplify-long-number -source-filename %s -pos=3:9 > %t.result/Float.swift
// RUN: diff -u %S/Outputs/Float.expected %t.result/Float.swift
// RUN: %refactor -simplify-long-number -source-filename %s -pos=4:11 > %t.result/PositiveInteger.swift
// RUN: diff -u %S/Outputs/PositiveInteger.expected %t.result/PositiveInteger.swift
// RUN: %refactor -simplify-long-number -source-filename %s -pos=5:7 > %t.result/NegativeInteger.swift
// RUN: diff -u %S/Outputs/NegativeInteger.expected %t.result/NegativeInteger.swift

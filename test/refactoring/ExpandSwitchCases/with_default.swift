enum E {
  case e1
  case e2
  case e3
  case e4
}

func foo(e: E) -> Int {
  switch e {
    default: return 3
   }
}
// RUN: %empty-directory(%t.result)
// RUN: %refactor -expand-switch-cases -source-filename %s -pos=9:8 >> %t.result/L10.swift
// RUN: diff -u %S/Outputs/with_default/L10.swift.expected %t.result/L10.swift

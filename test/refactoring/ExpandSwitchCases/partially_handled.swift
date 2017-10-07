enum E {
  case e1
  case e2
  case e3
  case e4
}

func foo(e: E) -> Int {
  switch e {
    case .e1: return 5
   }
}
// RUN: rm -rf %t.result && mkdir -p %t.result
// RUN: %refactor -expand-switch-cases -source-filename %s -pos=9:8 >> %t.result/L11.swift
// RUN: diff -u %S/Outputs/partially_handled/L11.swift.expected %t.result/L11.swift

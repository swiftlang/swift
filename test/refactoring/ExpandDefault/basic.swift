enum E {
  case e1
  case e2
  case e3
  case e4
  case e5
}

func foo(e: E) -> Int {
  switch(e) {
  case .e1:
    return 0
  case .e2:
    return 0
  default:
    return 1
  }
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor -expand-default -source-filename %s -pos=15:8 >> %t.result/L15.swift
// RUN: diff -u %S/Outputs/basic/L15.swift.expected %t.result/L15.swift

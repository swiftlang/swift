enum MyEnum {
  case Case1
  case Case2
  case Case3
}

func foo2(_ e : MyEnum) -> Int {
  switch e {
  case .Case1:
    break
  case .Case2:
    break
  case .Case3:
    break
  }
  switch e {
  case .Case1:
    return 1
  case .Case2:
    return 2
  case .Case3:
    return 3
  }
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor -extract-function -source-filename %s -pos=8:1 -end-pos=15:4 >> %t.result/Void.swift
// RUN: diff -u %S/Outputs/extract_switch/Void.swift.expected %t.result/Void.swift
// RUN: %empty-directory(%t.result)
// RUN: %refactor -extract-function -source-filename %s -pos=16:1 -end-pos=23:4 >> %t.result/Int.swift
// RUN: diff -u %S/Outputs/extract_switch/Int.swift.expected %t.result/Int.swift

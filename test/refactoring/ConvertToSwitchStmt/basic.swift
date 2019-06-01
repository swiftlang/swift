enum E {
  case case1
  case case2
  case case3
}

func someFunc(e: E) {
  if e == .case1 {
    print("case1")
  } else if e == .case2 {
    print("case2")
  }
  else {
    print("default")
  }
}

func twoStringBody(e: E) {
  if e == .case1 {
    print("first string")
    print("second string")
  } else {
    print("default")
  }
}

func withoutElse(e: E) {
  if e == .case1 {
    print("case1")
  } else if e == .case2 {
    print("case2")
  } else if e == .case3 {
    print("case3")
  }
}

// RUN: rm -rf %t.result && mkdir -p %t.result

// RUN: %refactor -convert-to-switch-stmt -source-filename %s -pos=8:3 -end-pos=15:4 > %t.result/L8-3.swift
// RUN: diff -u %S/Outputs/basic/L8-3.swift.expected %t.result/L8-3.swift

// RUN: %refactor -convert-to-switch-stmt -source-filename %s -pos=19:3 -end-pos=24:4 > %t.result/L19-3.swift
// RUN: diff -u %S/Outputs/basic/L19-3.swift.expected %t.result/L19-3.swift

// RUN: %refactor -convert-to-switch-stmt -source-filename %s -pos=28:3 -end-pos=34:4 > %t.result/L28-3.swift
// RUN: diff -u %S/Outputs/basic/L28-3.swift.expected %t.result/L28-3.swift

func test1() {
  if true {
    let x = 1
    print(x)
  } else {
    let x = 2
    print(x)
  }
}

func test2(arg1: Int?, arg2: (Int, String)?) {
  if let x = arg1 {
    print(x)
  } else if let (x, y) = arg2 {
    print(x, y)
  }
}

func test3(arg: Int?) {
  switch arg {
  case let .some(x) where x == 0:
    print(x)
  case let .some(x) where x == 1,
       let .some(x) where x == 2: // FIXME: This 'x' in '.some(x)' isn't properly renamed in 'casebind_2' case.
    print(x)
  default:
    break
  }
}

struct Err1 : Error { }
func test4(arg: () throws -> Void) {
  do {
    try arg()
  } catch let x as Err1 {
    print(x)
  } catch let x {
    print(x)
  }
}

func test5(_ x: Int) {
  let x = x 
  print(x)
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor -rename -source-filename %s -pos=3:9 -new-name="xRenamed" >> %t.result/localvar_1.swift
// RUN: %refactor -rename -source-filename %s -pos=7:11 -new-name="xRenamed" >> %t.result/localvar_2.swift
// RUN: diff -u %S/Outputs/local/localvar_1.swift.expected %t.result/localvar_1.swift
// RUN: diff -u %S/Outputs/local/localvar_2.swift.expected %t.result/localvar_2.swift
// RUN: %refactor -rename -source-filename %s -pos=12:10 -new-name="xRenamed" >> %t.result/ifbind_1.swift
// RUN: %refactor -rename -source-filename %s -pos=15:11 -new-name="xRenamed" >> %t.result/ifbind_2.swift
// RUN: diff -u %S/Outputs/local/ifbind_1.swift.expected %t.result/ifbind_1.swift
// RUN: diff -u %S/Outputs/local/ifbind_2.swift.expected %t.result/ifbind_2.swift
// RUN: %refactor -rename -source-filename %s -pos=21:18 -new-name="xRenamed" >> %t.result/casebind_1.swift
// RUN: %refactor -rename -source-filename %s -pos=25:11 -new-name="xRenamed" >> %t.result/casebind_2.swift
// RUN: diff -u %S/Outputs/local/casebind_1.swift.expected %t.result/casebind_1.swift
// RUN: diff -u %S/Outputs/local/casebind_2.swift.expected %t.result/casebind_2.swift
// RUN: %refactor -rename -source-filename %s -pos=35:15 -new-name="xRenamed" >> %t.result/catch_1.swift
// RUN: %refactor -rename -source-filename %s -pos=38:11 -new-name="xRenamed" >> %t.result/catch_2.swift
// RUN: diff -u %S/Outputs/local/catch_1.swift.expected %t.result/catch_1.swift
// RUN: diff -u %S/Outputs/local/catch_2.swift.expected %t.result/catch_2.swift
// RUN: %refactor -rename -source-filename %s -pos=42:14 -new-name="xRenamed" >> %t.result/param_1.swift
// RUN: %refactor -rename -source-filename %s -pos=44:9 -new-name="xRenamed" >> %t.result/param_2.swift
// RUN: diff -u %S/Outputs/local/param_1.swift.expected %t.result/param_1.swift
// RUN: diff -u %S/Outputs/local/param_2.swift.expected %t.result/param_2.swift

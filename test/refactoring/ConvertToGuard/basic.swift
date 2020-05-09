func foo(idxOpt: Int?) {
  if let idx = idxOpt {
    print(idx)
  }
}

func bar(fooOpt: Int?) {
  if let foo = fooOpt {
    let incrementedFoo = foo + 1
    print(incrementedFoo)
  }
}

func fooBar(fooOpt: Int?) {
  if let foo = fooOpt {
    let incrementedFoo = foo + 1
    print(incrementedFoo)
    print(foo)
  }
}

func barFoo(idxOpt: Int?) {
  if let idx = idxOpt {
    print(idx)
  } else {
    print("nil")
  }
}

// RUN: rm -rf %t.result && mkdir -p %t.result

// RUN: %refactor -convert-to-guard -source-filename %s -pos=2:3 -end-pos=4:4 > %t.result/L2-3.swift
// RUN: diff -u %S/Outputs/basic/L2-3.swift.expected %t.result/L2-3.swift

// RUN: %refactor -convert-to-guard -source-filename %s -pos=8:3 -end-pos=11:4 > %t.result/L8-3.swift
// RUN: diff -u %S/Outputs/basic/L8-3.swift.expected %t.result/L8-3.swift

// RUN: %refactor -convert-to-guard -source-filename %s -pos=15:3 -end-pos=19:4 > %t.result/L15-3.swift
// RUN: diff -u %S/Outputs/basic/L15-3.swift.expected %t.result/L15-3.swift

// RUN: %refactor -convert-to-guard -source-filename %s -pos=23:3 -end-pos=27:4 > %t.result/L23-3.swift
// RUN: diff -u %S/Outputs/basic/L23-3.swift.expected %t.result/L23-3.swift

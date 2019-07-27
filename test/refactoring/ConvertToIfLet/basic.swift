func foo(idxOpt: Int?) {
  guard let idx = idxOpt else {
    return
  }
  print(idx)
}

func bar(fooOpt: Int?) {
  guard let foo = fooOpt else {
    return
  }
  let incrementedFoo = foo + 1
  print(incrementedFoo)
}

func fooBar(idxOpt: Int?) {
  guard let idx = idxOpt else {
    print("nil")
    return
  }
  print(idx)
}

// RUN: rm -rf %t.result && mkdir -p %t.result

// RUN: %refactor -convert-to-iflet -source-filename %s -pos=2:3 -end-pos=5:13 > %t.result/L2-3.swift
// RUN: diff -u %S/Outputs/basic/L2-3.swift.expected %t.result/L2-3.swift

// RUN: %refactor -convert-to-iflet -source-filename %s -pos=9:3 -end-pos=13:24 > %t.result/L9-3.swift
// RUN: diff -u %S/Outputs/basic/L9-3.swift.expected %t.result/L9-3.swift

// RUN: %refactor -convert-to-iflet -source-filename %s -pos=17:3 -end-pos=21:13 > %t.result/L17-3.swift
// RUN: diff -u %S/Outputs/basic/L17-3.swift.expected %t.result/L17-3.swift

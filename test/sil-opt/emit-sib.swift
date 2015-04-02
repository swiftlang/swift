// RUN: rm -rf %t && mkdir -p %t

// RUN: %target-swiftc_driver -emit-sib %s -module-name test -O -o %t/a-opt.sib
// RUN: %target-swiftc_driver -emit-ir %t/a-opt.sib -module-name test -o %t/test.ll
// RUN: mv %t/test.ll %t/a-test.ll

// RUN: %target-swiftc_driver -emit-sibgen %s -module-name test -o %t/b-sibgen.sib
// RUN: %target-sil-opt -emit-sib %t/b-sibgen.sib -module-name test -diagnostics -o %t/b-sibgen-diag.sib
// RUN: %target-sil-opt -emit-sib %t/b-sibgen-diag.sib -module-name test -performance -o %t/b-opt.sib
// RUN: %target-swiftc_driver -emit-ir %t/b-opt.sib -module-name test -o %t/test.ll
// RUN: mv %t/test.ll %t/b-test.ll

// RUN: cmp %t/a-test.ll %t/b-test.ll

func test123() -> () {
  println("test123")
}

println("Hello World!")
test123()

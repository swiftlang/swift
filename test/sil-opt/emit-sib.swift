// <rdar://problem/27394237> IR generation CFG order isn't deterministic
XFAIL: *

// RUN: rm -rf %t && mkdir -p %t

// RUN: %target-swiftc_driver -emit-sib %s -module-name test -assert-config Release -O -o %t/a-opt.sib
// RUN: %target-swiftc_driver -emit-ir %t/a-opt.sib -module-name test -assert-config Release -o %t/test.ll
// RUN: mv %t/test.ll %t/a-test.ll

// RUN: %target-swiftc_driver -emit-sibgen %s -module-name test -assert-config Release -o %t/b-sibgen.sib
// RUN: %target-sil-opt -emit-sib %t/b-sibgen.sib -module-name test -assert-conf-id=1 -diagnostics -o %t/b-sibgen-diag.sib
// RUN: %target-sil-opt -emit-sib %t/b-sibgen-diag.sib -module-name test -assert-conf-id=1 -O -o %t/b-opt.sib
// RUN: %target-swiftc_driver -emit-ir %t/b-opt.sib -module-name test -assert-config Release -o %t/test.ll
// RUN: mv %t/test.ll %t/b-test.ll

// RN: cmp %t/a-test.ll %t/b-test.ll

func test123() -> () {
  print("test123")
}

print("Hello World!")
test123()

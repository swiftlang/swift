// RUN: mkdir -p %t.out
// RUN: echo "{\"%S/Inputs/t1.swift\": {\"remap\": \"%t.out/t1.remap\"}, \"%S/Inputs/t2.swift\": {\"remap\": \"%t.out/t2.remap\"}}" > %t.json
// RUN: not %swiftc_driver -target %target-triple -module-name Blah -fixit-code %S/Inputs/t1.swift %S/Inputs/t2.swift -emit-module -emit-module-path %t.mod -emit-objc-header -emit-objc-header-path %t.h -j 1 -output-file-map %t.json 2> %t.err.txt
// RUN: c-arcmt-test %t.out/t1.remap %t.out/t2.remap | arcmt-test -verify-transformed-files %S/Inputs/t1.swift.result %S/Inputs/t2.swift.result
// RUN: FileCheck --input-file=%t.err.txt %s

// CHECK: error: use of unresolved identifier 'undeclared_foo1'
// CHECK: error: use of unresolved identifier 'undeclared_foo2'

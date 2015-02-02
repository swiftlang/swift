// RUN: mkdir -p %t.out
// RUN: echo "{\"%S/Inputs/t1.swift\": {\"remap\": \"%t.out/t1.remap\"}, \"%S/Inputs/t2.swift\": {\"remap\": \"%t.out/t2.remap\"}}" > %t.json
// RUN: not %swiftc_driver -target %target-triple -module-name Blah -update-code %S/Inputs/t1.swift %S/Inputs/t2.swift -emit-module -emit-module-path %t.mod -emit-objc-header -emit-objc-header-path %t.h -j 1 -output-file-map %t.json
// RUN: c-arcmt-test %t.out/t1.remap %t.out/t2.remap | arcmt-test -verify-transformed-files %S/Inputs/t1.swift.result %S/Inputs/t2.swift.result
// c-index-test -read-diagnostics %t.dia > %t.deserialized_diagnostics.txt 2>&1
// FileCheck --input-file=%t.deserialized_diagnostics.txt %s

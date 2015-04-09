// REQUIRES: objc_interop
// FIXME: actually, this test requires Mach-O.

// RUN: rm -rf %t && mkdir -p %t/tmp/ && touch %t/tmp/dummy
// RUN: env TMPDIR=%t/tmp/ %target-swiftc_driver -emit-executable %s -o %t/main
// RUN: ls %t/main
// RUN: ls %t/tmp | FileCheck -check-prefix=EMPTY %s

// XFAIL: linux

// EMPTY-NOT: .{{(o|swiftmodule|swiftdoc)}}

// RUN: rm -rf %t && mkdir -p %t/tmp/ && touch %t/tmp/dummy
// RUN: env TMPDIR=%t/tmp/ %target-swiftc_driver -emit-executable %s -o %t/main2 -emit-module-path %t/main2.swiftmodule
// RUN: ls %t/main2
// RUN: ls %t/main2.swiftmodule
// RUN: ls %t/tmp | FileCheck -check-prefix=EMPTY %s

// RUN: rm -rf %t && mkdir -p %t/tmp/ && touch %t/tmp/dummy
// RUN: env TMPDIR=%t/tmp/ %target-swiftc_driver -emit-executable %s -o %t/main3 -g
// RUN: ls %t/main3
// RUN: ls %t/tmp | FileCheck -check-prefix=EMPTY %s

// RUN: rm -rf %t && mkdir -p %t/tmp/ && touch %t/tmp/dummy
// RUN: env TMPDIR=%t/tmp/ %target-swiftc_driver -emit-executable %s -o %t/main4 -emit-module-path %t/main4.swiftmodule -g
// RUN: ls %t/main4
// RUN: ls %t/main4.swiftmodule
// RUN: ls %t/main4.dSYM
// RUN: ls %t/tmp | FileCheck -check-prefix=EMPTY %s

// RUN: rm -rf %t && mkdir -p %t/tmp/ && touch %t/tmp/dummy
// RUN: echo "{\"%s\": {\"object\": \"%t/main5.o\"}}" > %t.json
// RUN: env TMPDIR=%t/tmp/ %target-swiftc_driver -emit-executable %s -o %t/main5 -output-file-map %t.json -g
// RUN: ls %t/main5
// RUN: ls %t/main5.o
// RUN: ls %t/main5.dSYM
// RUN: ls %t/tmp | FileCheck -check-prefix=EMPTY %s

// RUN: rm -rf %t && mkdir -p %t/tmp/ && touch %t/tmp/dummy
// RUN: env TMPDIR=%t/tmp/ %target-swiftc_driver -emit-executable %s -o %t/main6 -g -save-temps
// RUN: ls %t/main6
// RUN: ls %t/main6.dSYM
// RUN: ls %t/tmp | FileCheck -check-prefix=SAVE-TEMPS %s

// SAVE-TEMPS-DAG: temp-files-{{.+}}.o
// SAVE-TEMPS-DAG: temp-files-{{.+}}.swiftmodule
// SAVE-TEMPS-DAG: main6-{{.+}}.swiftmodule
// SAVE-TEMPS-NOT: .{{(o|swiftmodule|swiftdoc)}}

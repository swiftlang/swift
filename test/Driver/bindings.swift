// RUN: %empty-directory(%t)

// RUN: %swiftc_driver -driver-print-bindings -target x86_64-apple-macosx10.9 %/s 2>&1 | %FileCheck %s -check-prefix=BASIC
// BASIC: # "x86_64-apple-macosx10.9" - "swift{{c?(\.EXE)?}}", inputs: ["{{.*}}bindings.swift"], output: {object: "[[OBJECT:.*\.o]]"}
// BASIC: # "x86_64-apple-macosx10.9" - "ld", inputs: ["[[OBJECT]]"], output: {image: "bindings"}

// RUN: %swiftc_driver -driver-print-bindings -target x86_64-apple-macosx10.9 - 2>&1 | %FileCheck %s -check-prefix=STDIN
// STDIN: # "x86_64-apple-macosx10.9" - "swift{{c?(\.EXE)?}}", inputs: ["-"], output: {object: "[[OBJECT:.*\.o]]"}
// STDIN: # "x86_64-apple-macosx10.9" - "ld", inputs: ["[[OBJECT]]"], output: {image: "main"}

// RUN: %swiftc_driver -driver-print-bindings -target x86_64-apple-macosx10.9 %/S/Inputs/invalid-module-name.swift 2>&1 | %FileCheck %s -check-prefix=INVALID-NAME-SINGLE-FILE
// INVALID-NAME-SINGLE-FILE: # "x86_64-apple-macosx10.9" - "swift{{c?(\.EXE)?}}", inputs: ["{{.*}}/Inputs/invalid-module-name.swift"], output: {object: "[[OBJECT:.*\.o]]"}
// INVALID-NAME-SINGLE-FILE: # "x86_64-apple-macosx10.9" - "ld", inputs: ["[[OBJECT]]"], output: {image: "invalid-module-name"}

// RUN: %swiftc_driver -driver-print-bindings -target x86_64-apple-macosx10.9 -o NamedOutput %/s 2>&1 | %FileCheck %s -check-prefix=NAMEDIMG
// RUN: %swiftc_driver -driver-print-bindings -target x86_64-apple-macosx10.9 -module-name NamedOutput %/s 2>&1 | %FileCheck %s -check-prefix=NAMEDIMG
// NAMEDIMG: # "x86_64-apple-macosx10.9" - "swift{{c?(\.EXE)?}}", inputs: ["{{.*}}bindings.swift"], output: {object: "[[OBJECT:.*\.o]]"}
// NAMEDIMG: # "x86_64-apple-macosx10.9" - "ld", inputs: ["[[OBJECT]]"], output: {image: "NamedOutput"}

// RUN: %swiftc_driver -driver-print-bindings -target x86_64-apple-macosx10.9 -c %/s 2>&1 | %FileCheck %s -check-prefix=OBJ
// OBJ: # "x86_64-apple-macosx10.9" - "swift{{c?(\.EXE)?}}", inputs: ["{{.*}}bindings.swift"], output: {object: "bindings.o"}

// RUN: %swiftc_driver -driver-print-bindings -target x86_64-apple-macosx10.9 -c %/s -o /build/bindings.o 2>&1 | %FileCheck %s -check-prefix=NAMEDOBJ
// NAMEDOBJ: # "x86_64-apple-macosx10.9" - "swift{{c?(\.EXE)?}}", inputs: ["{{.*}}bindings.swift"], output: {object: "/build/bindings.o"}

// RUN: %swiftc_driver -driver-print-bindings -target x86_64-apple-macosx10.9 -emit-sil %/s 2>&1 | %FileCheck %s -check-prefix=SIL
// SIL: # "x86_64-apple-macosx10.9" - "swift{{c?(\.EXE)?}}", inputs: ["{{.*}}bindings.swift"], output: {sil: "-"}

// RUN: %swiftc_driver -driver-print-bindings -target x86_64-apple-macosx10.9 -emit-ir %S/Inputs/empty.sil 2>&1 | %FileCheck %s -check-prefix=SIL-INPUT
// SIL-INPUT: # "x86_64-apple-macosx10.9" - "swift{{c?(\.EXE)?}}", inputs: ["{{.*}}empty.sil"], output: {llvm-ir: "-"}

// RUN: %swiftc_driver -driver-print-bindings -target x86_64-apple-macosx10.9 -c -incremental %/s 2>&1 | %FileCheck %s -check-prefix=OBJ-AND-DEPS
// OBJ-AND-DEPS: # "x86_64-apple-macosx10.9" - "swift{{c?(\.EXE)?}}", inputs: ["{{.*}}bindings.swift"], output: {
// OBJ-AND-DEPS-DAG: swift-dependencies: "bindings.swiftdeps"
// OBJ-AND-DEPS-DAG: object: "bindings.o"
// OBJ-AND-DEPS: }

// RUN: echo '{"%/s": {"object": "objroot/bindings.o"}}' > %t/map.json
// RUN: %swiftc_driver -driver-print-bindings -output-file-map %t/map.json -target x86_64-apple-macosx10.9 %/s 2>&1 | %FileCheck %s -check-prefix=MAP
// MAP: # "x86_64-apple-macosx10.9" - "swift{{c?(\.EXE)?}}", inputs: ["{{.*}}bindings.swift"], output: {object: "objroot/bindings.o"}

// RUN: echo '{"": {"object": "objroot/bindings.o"}}' > %t/map.json
// RUN: %swiftc_driver -driver-print-bindings -output-file-map %t/map.json -whole-module-optimization -target x86_64-apple-macosx10.9 %/s %S/Inputs/lib.swift 2>&1 | %FileCheck %s -check-prefix=MAP-WFO
// MAP-WFO: # "x86_64-apple-macosx10.9" - "swift{{c?(\.EXE)?}}", inputs: ["{{.*}}bindings.swift", "{{.*}}lib.swift"], output: {object: "objroot/bindings.o"}

// RUN: touch %t/a.o %t/b.o
// RUN: %swiftc_driver -driver-print-bindings -target x86_64-apple-macosx10.9 %t/a.o %t/b.o -o main 2>&1 | %FileCheck %s -check-prefix=LINK-ONLY
// RUN: %swiftc_driver -driver-print-bindings -target x86_64-apple-macosx10.9 -g %t/a.o %t/b.o -o main 2>&1 | %FileCheck %s -check-prefix=LINK-ONLY
// LINK-ONLY: # "x86_64-apple-macosx10.9" - "ld", inputs: ["{{.*}}/a.o", "{{.*}}/b.o"], output: {image: "main"}

// RUN: touch %t/a.swiftmodule %t/b.swiftmodule
// RUN: %swiftc_driver -driver-print-bindings -target x86_64-apple-macosx10.9 -g %t/a.o %t/b.o %t/a.swiftmodule %t/b.swiftmodule -o main 2>&1 | %FileCheck %s -check-prefix=DEBUG-LINK-ONLY
// DEBUG-LINK-ONLY-NOT: "swift"
// DEBUG-LINK-ONLY: # "x86_64-apple-macosx10.9" - "ld", inputs: ["{{.*}}/a.o", "{{.*}}/b.o", "{{.*}}/a.swiftmodule", "{{.*}}/b.swiftmodule"], output: {image: "main"}

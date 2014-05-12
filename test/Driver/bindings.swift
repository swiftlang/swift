// RUN: %swift_driver -driver-print-bindings -target x86_64-apple-darwin13.0.0 %s 2>&1 | FileCheck %s -check-prefix=BASIC
// BASIC: # "x86_64-apple-darwin13.0.0" - "swift", inputs: ["{{.*}}bindings.swift"], output: {object: "[[OBJECT:.*\.o]]"}
// BASIC: # "x86_64-apple-darwin13.0.0" - "darwin::Linker", inputs: ["[[OBJECT]]"], output: {image: "bindings"}

// RUN: %swift_driver -driver-print-bindings -target x86_64-apple-darwin13.0.0 - 2>&1 | FileCheck %s -check-prefix=STDIN
// STDIN: # "x86_64-apple-darwin13.0.0" - "swift", inputs: ["-"], output: {object: "[[OBJECT:.*\.o]]"}
// STDIN: # "x86_64-apple-darwin13.0.0" - "darwin::Linker", inputs: ["[[OBJECT]]"], output: {image: "main"}

// RUN: %swift_driver -driver-print-bindings -target x86_64-apple-darwin13.0.0 %S/Inputs/invalid-module-name.swift 2>&1 | FileCheck %s -check-prefix=INVALID-NAME-SINGLE-FILE
// INVALID-NAME-SINGLE-FILE: # "x86_64-apple-darwin13.0.0" - "swift", inputs: ["{{.*}}/Inputs/invalid-module-name.swift"], output: {object: "[[OBJECT:.*\.o]]"}
// INVALID-NAME-SINGLE-FILE: # "x86_64-apple-darwin13.0.0" - "darwin::Linker", inputs: ["[[OBJECT]]"], output: {image: "invalid-module-name"}

// RUN: %swift_driver -driver-print-bindings -target x86_64-apple-darwin13.0.0 -o NamedOutput %s 2>&1 | FileCheck %s -check-prefix=NAMEDIMG
// RUN: %swift_driver -driver-print-bindings -target x86_64-apple-darwin13.0.0 -module-name NamedOutput %s 2>&1 | FileCheck %s -check-prefix=NAMEDIMG
// NAMEDIMG: # "x86_64-apple-darwin13.0.0" - "swift", inputs: ["{{.*}}bindings.swift"], output: {object: "[[OBJECT:.*\.o]]"}
// NAMEDIMG: # "x86_64-apple-darwin13.0.0" - "darwin::Linker", inputs: ["[[OBJECT]]"], output: {image: "NamedOutput"}

// RUN: %swift_driver -driver-print-bindings -target x86_64-apple-darwin13.0.0 -c %s 2>&1 | FileCheck %s -check-prefix=OBJ
// OBJ: # "x86_64-apple-darwin13.0.0" - "swift", inputs: ["{{.*}}bindings.swift"], output: {object: "bindings.o"}

// RUN: %swift_driver -driver-print-bindings -target x86_64-apple-darwin13.0.0 -c %s -o /build/bindings.o 2>&1 | FileCheck %s -check-prefix=NAMEDOBJ
// NAMEDOBJ: # "x86_64-apple-darwin13.0.0" - "swift", inputs: ["{{.*}}bindings.swift"], output: {object: "/build/bindings.o"}

// RUN: %swift_driver -driver-print-bindings -target x86_64-apple-darwin13.0.0 -emit-sil %s 2>&1 | FileCheck %s -check-prefix=SIL
// SIL: # "x86_64-apple-darwin13.0.0" - "swift", inputs: ["{{.*}}bindings.swift"], output: {sil: "-"}

// RUN: %swift_driver -driver-print-bindings -target x86_64-apple-darwin13.0.0 -emit-ir %S/Inputs/empty.sil 2>&1 | FileCheck %s -check-prefix=SIL-INPUT
// SIL-INPUT: # "x86_64-apple-darwin13.0.0" - "swift", inputs: ["{{.*}}empty.sil"], output: {llvm-ir: "-"}

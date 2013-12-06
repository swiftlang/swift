// RUN: %swift_driver -driver-print-bindings -target x86_64-apple-darwin13.0.0 %s 2>&1 | FileCheck %s -check-prefix=BASIC
// BASIC: # "x86_64-apple-darwin13.0.0" - "swift", inputs: ["{{.*}}bindings.swift"], output: "{{.*}}.o"
// BASIC: # "x86_64-apple-darwin13.0.0" - "darwin::Linker", inputs: ["{{.*}}.o"], output: "a.out"

// RUN: %swift_driver -driver-print-bindings -target x86_64-apple-darwin13.0.0 -o named-output %s 2>&1 | FileCheck %s -check-prefix=NAMEDIMG
// NAMEDIMG: # "x86_64-apple-darwin13.0.0" - "swift", inputs: ["{{.*}}bindings.swift"], output: "{{.*}}.o"
// NAMEDIMG: # "x86_64-apple-darwin13.0.0" - "darwin::Linker", inputs: ["{{.*}}.o"], output: "named-output"

// RUN: %swift_driver -driver-print-bindings -target x86_64-apple-darwin13.0.0 -c %s 2>&1 | FileCheck %s -check-prefix=OBJ
// OBJ: # "x86_64-apple-darwin13.0.0" - "swift", inputs: ["{{.*}}bindings.swift"], output: "bindings.o"

// RUN: %swift_driver -driver-print-bindings -target x86_64-apple-darwin13.0.0 -c %s -o /build/bindings.o 2>&1 | FileCheck %s -check-prefix=NAMEDOBJ
// NAMEDOBJ: # "x86_64-apple-darwin13.0.0" - "swift", inputs: ["{{.*}}bindings.swift"], output: "/build/bindings.o"

// RUN: %swift_driver -driver-print-bindings -target x86_64-apple-darwin13.0.0 -emit-sil %s 2>&1 | FileCheck %s -check-prefix=SIL
// SIL: # "x86_64-apple-darwin13.0.0" - "swift", inputs: ["{{.*}}bindings.swift"], output: "bindings.sil"

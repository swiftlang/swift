// RUN: echo "{\"%s\": {\"object\": \"/build/obj/advanced_output_file_map.o\", \"swiftmodule\": \"/build/swiftmodule/advanced_output_file_map.swiftmodule\", \"diagnostics\": \"/build/dia/advanced_output_file_map.dia\"}, \"%S/Inputs/main.swift\": {\"object\": \"/build/obj/main.o\", \"swiftmodule\": \"/build/swiftmodule/main.swiftmodule\", \"diagnostics\": \"/build/dia/main.dia\"}, \"%S/Inputs/lib.swift\": {\"object\": \"/build/obj/lib.o\", \"swiftmodule\": \"/build/swiftmodule/lib.swiftmodule\", \"diagnostics\": \"/build/dia/lib.dia\"}}" > %t.json

// RUN: %swift_driver -driver-print-output-file-map -target x86_64-apple-darwin13.0.0 -emit-executable -emit-module -serialize-diagnostics %s %S/Inputs/main.swift %S/Inputs/lib.swift -o /build/advanced_output_file_map.out -emit-module-path /build/OutputFileMap.swiftmodule -module-name OutputFileMap -output-file-map %t.json 2>&1 | FileCheck %s -check-prefix=DUMPOFM
// RUN: %swift_driver -driver-print-bindings -target x86_64-apple-darwin13.0.0 -emit-executable -emit-module -serialize-diagnostics %s %S/Inputs/main.swift %S/Inputs/lib.swift -o /build/advanced_output_file_map.out -emit-module-path /build/OutputFileMap.swiftmodule -module-name OutputFileMap -output-file-map %t.json 2>&1 | FileCheck %s -check-prefix=BINDINGS

// DUMPOFM: {{.*}}/Inputs/lib.swift -> object: "/build/obj/lib.o"
// DUMPOFM-NEXT: {{.*}}/Inputs/lib.swift -> swiftmodule: "/build/swiftmodule/lib.swiftmodule"
// DUMPOFM-NEXT: {{.*}}/Inputs/lib.swift -> diagnostics: "/build/dia/lib.dia"
// DUMPOFM-NEXT: {{.*}}/Inputs/main.swift -> object: "/build/obj/main.o"
// DUMPOFM-NEXT: {{.*}}/Inputs/main.swift -> swiftmodule: "/build/swiftmodule/main.swiftmodule"
// DUMPOFM-NEXT: {{.*}}/Inputs/main.swift -> diagnostics: "/build/dia/main.dia"
// DUMPOFM-NEXT: {{.*}}/advanced_output_file_map.swift -> object: "/build/obj/advanced_output_file_map.o"
// DUMPOFM-NEXT: {{.*}}/advanced_output_file_map.swift -> swiftmodule: "/build/swiftmodule/advanced_output_file_map.swiftmodule"
// DUMPOFM-NEXT: {{.*}}/advanced_output_file_map.swift -> diagnostics: "/build/dia/advanced_output_file_map.dia"

// BINDINGS: # "x86_64-apple-darwin13.0.0" - "swift", inputs: ["{{.*}}/advanced_output_file_map.swift"], output: {object: "/build/obj/advanced_output_file_map.o", swiftmodule: "/build/swiftmodule/advanced_output_file_map.swiftmodule", diagnostics: "/build/dia/advanced_output_file_map.dia"}
// BINDINGS: # "x86_64-apple-darwin13.0.0" - "swift", inputs: ["{{.*}}/Inputs/main.swift"], output: {object: "/build/obj/main.o", swiftmodule: "/build/swiftmodule/main.swiftmodule", diagnostics: "/build/dia/main.dia"}
// BINDINGS: # "x86_64-apple-darwin13.0.0" - "swift", inputs: ["{{.*}}/Inputs/lib.swift"], output: {object: "/build/obj/lib.o", swiftmodule: "/build/swiftmodule/lib.swiftmodule", diagnostics: "/build/dia/lib.dia"}
// BINDINGS: # "x86_64-apple-darwin13.0.0" - "merge-module", inputs: ["/build/obj/advanced_output_file_map.o", "/build/obj/main.o", "/build/obj/lib.o"], output: {swiftmodule: "/build/OutputFileMap.swiftmodule"}
// BINDINGS: # "x86_64-apple-darwin13.0.0" - "darwin::Linker", inputs: ["/build/obj/advanced_output_file_map.o", "/build/obj/main.o", "/build/obj/lib.o", "/build/OutputFileMap.swiftmodule"], output: {image: "/build/advanced_output_file_map.out"}

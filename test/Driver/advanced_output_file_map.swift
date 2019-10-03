// RUN: echo "{\"%/s\": {\"object\": \"/build/obj/advanced_output_file_map.o\", \"swiftmodule\": \"/build/swiftmodule/advanced_output_file_map.swiftmodule\", \"swiftdoc\": "/build/swiftmodule/advanced_output_file_map_x.swiftdoc", \"diagnostics\": \"/build/dia/advanced_output_file_map.dia\", \"dependencies\": \"/build/d/advanced_output_file_map.d\"}, \"%/S/Inputs/main.swift\": {\"object\": \"/build/obj/main.o\", \"swiftmodule\": \"/build/swiftmodule/main.swiftmodule\", \"swiftdoc\": "/build/swiftmodule/main_x.swiftdoc", \"diagnostics\": \"/build/dia/main.dia\", \"dependencies\": \"/build/d/main.d\"}, \"%/S/Inputs/lib.swift\": {\"object\": \"/build/obj/lib.o\", \"swiftmodule\": \"/build/swiftmodule/lib.swiftmodule\", \"swiftdoc\": \"/build/swiftmodule/lib_x.swiftdoc\", \"diagnostics\": \"/build/dia/lib.dia\", \"dependencies\": \"/build/d/lib.d\"}}" > %t.json

// RUN: %swiftc_driver -driver-print-output-file-map -target x86_64-apple-macosx10.9 -emit-executable -emit-module -serialize-diagnostics %/s %/S/Inputs/main.swift %/S/Inputs/lib.swift -g -o /build/advanced_output_file_map.out -emit-module-path /build/OutputFileMap.swiftmodule -module-name OutputFileMap -output-file-map %t.json 2>&1 | %FileCheck %/s -check-prefix=DUMPOFM
// RUN: %swiftc_driver -driver-print-bindings -target x86_64-apple-macosx10.9 -emit-executable -emit-module -serialize-diagnostics -emit-dependencies %/s %/S/Inputs/main.swift %/S/Inputs/lib.swift -g -o /build/advanced_output_file_map.out -emit-module-path /build/OutputFileMap.swiftmodule -module-name OutputFileMap -output-file-map %t.json 2>&1 | %FileCheck %/s -check-prefix=BINDINGS

// DUMPOFM: {{.*}}/Inputs/lib.swift -> object: "/build/obj/lib.o"
// DUMPOFM-NEXT: {{.*}}/Inputs/lib.swift -> dependencies: "/build/d/lib.d"
// DUMPOFM-NEXT: {{.*}}/Inputs/lib.swift -> swiftmodule: "/build/swiftmodule/lib.swiftmodule"
// DUMPOFM-NEXT: {{.*}}/Inputs/lib.swift -> swiftdoc: "/build/swiftmodule/lib_x.swiftdoc"
// DUMPOFM-NEXT: {{.*}}/Inputs/lib.swift -> diagnostics: "/build/dia/lib.dia"
// DUMPOFM-NEXT: {{.*}}/Inputs/main.swift -> object: "/build/obj/main.o"
// DUMPOFM-NEXT: {{.*}}/Inputs/main.swift -> dependencies: "/build/d/main.d"
// DUMPOFM-NEXT: {{.*}}/Inputs/main.swift -> swiftmodule: "/build/swiftmodule/main.swiftmodule"
// DUMPOFM-NEXT: {{.*}}/Inputs/main.swift -> swiftdoc: "/build/swiftmodule/main_x.swiftdoc"
// DUMPOFM-NEXT: {{.*}}/Inputs/main.swift -> diagnostics: "/build/dia/main.dia"
// DUMPOFM-NEXT: {{.*}}/advanced_output_file_map.swift -> object: "/build/obj/advanced_output_file_map.o"
// DUMPOFM-NEXT: {{.*}}/advanced_output_file_map.swift -> dependencies: "/build/d/advanced_output_file_map.d"
// DUMPOFM-NEXT: {{.*}}/advanced_output_file_map.swift -> swiftmodule: "/build/swiftmodule/advanced_output_file_map.swiftmodule"
// DUMPOFM-NEXT: {{.*}}/advanced_output_file_map.swift -> swiftdoc: "/build/swiftmodule/advanced_output_file_map_x.swiftdoc"
// DUMPOFM-NEXT: {{.*}}/advanced_output_file_map.swift -> diagnostics: "/build/dia/advanced_output_file_map.dia"

// BINDINGS: # "x86_64-apple-macosx10.9" - "swift{{c?(\.EXE)?}}", inputs: ["{{.*}}/advanced_output_file_map.swift"], output: {object: "/build/obj/advanced_output_file_map.o", dependencies: "/build/d/advanced_output_file_map.d", swiftmodule: "/build/swiftmodule/advanced_output_file_map.swiftmodule", swiftdoc: "/build/swiftmodule/advanced_output_file_map_x.swiftdoc", swiftsourceinfo: "/build/swiftmodule{{[/\\]}}advanced_output_file_map.swiftsourceinfo", diagnostics: "/build/dia/advanced_output_file_map.dia"}
// BINDINGS: # "x86_64-apple-macosx10.9" - "swift{{c?(\.EXE)?}}", inputs: ["{{.*}}/Inputs/main.swift"], output: {object: "/build/obj/main.o", dependencies: "/build/d/main.d", swiftmodule: "/build/swiftmodule/main.swiftmodule", swiftdoc: "/build/swiftmodule/main_x.swiftdoc", swiftsourceinfo: "/build/swiftmodule{{[/\\]}}main.swiftsourceinfo", diagnostics: "/build/dia/main.dia"}
// BINDINGS: # "x86_64-apple-macosx10.9" - "swift{{c?(\.EXE)?}}", inputs: ["{{.*}}/Inputs/lib.swift"], output: {object: "/build/obj/lib.o", dependencies: "/build/d/lib.d", swiftmodule: "/build/swiftmodule/lib.swiftmodule", swiftdoc: "/build/swiftmodule/lib_x.swiftdoc", swiftsourceinfo: "/build/swiftmodule{{[/\\]}}lib.swiftsourceinfo", diagnostics: "/build/dia/lib.dia"}
// BINDINGS: # "x86_64-apple-macosx10.9" - "swift{{c?(\.EXE)?}}", inputs: ["/build/obj/advanced_output_file_map.o", "/build/obj/main.o", "/build/obj/lib.o"], output: {swiftmodule: "/build/OutputFileMap.swiftmodule", swiftdoc: "/build{{[/\\]}}OutputFileMap.swiftdoc", swiftsourceinfo: "/build{{[/\\]}}OutputFileMap.swiftsourceinfo"}
// BINDINGS: # "x86_64-apple-macosx10.9" - "ld{{(.exe)?}}", inputs: ["/build/obj/advanced_output_file_map.o", "/build/obj/main.o", "/build/obj/lib.o", "/build/OutputFileMap.swiftmodule"], output: {image: "/build/advanced_output_file_map.out"}
// BINDINGS: # "x86_64-apple-macosx10.9" - "dsymutil{{(\.exe)?}}", inputs: ["/build/advanced_output_file_map.out"], output: {dSYM: "/build/advanced_output_file_map.out.dSYM"}

// Test both ways: -disable-only-one-dependency-file, and the default which should be same as -enable-only-one-dependency-file

// RUN: %empty-directory(%t)

// Create an output file map
// RUN: echo "{\"%/s\": {\"object\": \"./obj/advanced_output_file_map.o\", \"swiftmodule\": \"./swiftmodule/advanced_output_file_map.swiftmodule\", \"swiftdoc\": "./swiftmodule/advanced_output_file_map_x.swiftdoc", \"diagnostics\": \"./dia/advanced_output_file_map.dia\", \"dependencies\": \"./d/advanced_output_file_map.d\"}, " >%t/ofm.json
// RUN: echo " \"%/S/Inputs/main.swift\": {\"object\": \"./obj/main.o\", \"swiftmodule\": \"./swiftmodule/main.swiftmodule\", \"swiftdoc\": \"./swiftmodule/main_x.swiftdoc\", \"diagnostics\": \"./dia/main.dia\", \"dependencies\": \"./d/main.d\"}, " >> %t/ofm.json
// RUN: echo " \"%/S/Inputs/lib.swift\":  {\"object\": \"./obj/lib.o\",  \"swiftmodule\": \"./swiftmodule/lib.swiftmodule\",  \"swiftdoc\": \"./swiftmodule/lib_x.swiftdoc\",  \"diagnostics\": \"./dia/lib.dia\",  \"dependencies\": \"./d/lib.d\"}}" >> %t/ofm.json

// With -disable-only-one-dependency-file

// RUN: cd %t && %swiftc_driver -disable-only-one-dependency-file -driver-print-output-file-map -target x86_64-apple-macosx10.9 -emit-executable -emit-module -serialize-diagnostics %/s %/S/Inputs/main.swift %/S/Inputs/lib.swift -g -o ./advanced_output_file_map.out -emit-module-path ./OutputFileMap.swiftmodule -module-name OutputFileMap -output-file-map %t/ofm.json 2>&1 | %FileCheck %/s -check-prefix=DUMPOFM-DIS

// DUMPOFM-DIS: {{.*}}/Inputs/lib.swift -> object: "./obj/lib.o"
// DUMPOFM-DIS-NEXT: {{.*}}/Inputs/lib.swift -> dependencies: "./d/lib.d"
// DUMPOFM-DIS-NEXT: {{.*}}/Inputs/lib.swift -> swiftmodule: "./swiftmodule/lib.swiftmodule"
// DUMPOFM-DIS-NEXT: {{.*}}/Inputs/lib.swift -> swiftdoc: "./swiftmodule/lib_x.swiftdoc"
// DUMPOFM-DIS-NEXT: {{.*}}/Inputs/lib.swift -> diagnostics: "./dia/lib.dia"
// DUMPOFM-DIS-NEXT: {{.*}}/Inputs/main.swift -> object: "./obj/main.o"
// DUMPOFM-DIS-NEXT: {{.*}}/Inputs/main.swift -> dependencies: "./d/main.d"
// DUMPOFM-DIS-NEXT: {{.*}}/Inputs/main.swift -> swiftmodule: "./swiftmodule/main.swiftmodule"
// DUMPOFM-DIS-NEXT: {{.*}}/Inputs/main.swift -> swiftdoc: "./swiftmodule/main_x.swiftdoc"
// DUMPOFM-DIS-NEXT: {{.*}}/Inputs/main.swift -> diagnostics: "./dia/main.dia"
// DUMPOFM-DIS-NEXT: {{.*}}/advanced_output_file_map.swift -> object: "./obj/advanced_output_file_map.o"
// DUMPOFM-DIS-NEXT: {{.*}}/advanced_output_file_map.swift -> dependencies: "./d/advanced_output_file_map.d"
// DUMPOFM-DIS-NEXT: {{.*}}/advanced_output_file_map.swift -> swiftmodule: "./swiftmodule/advanced_output_file_map.swiftmodule"
// DUMPOFM-DIS-NEXT: {{.*}}/advanced_output_file_map.swift -> swiftdoc: "./swiftmodule/advanced_output_file_map_x.swiftdoc"
// DUMPOFM-DIS-NEXT: {{.*}}/advanced_output_file_map.swift -> diagnostics: "./dia/advanced_output_file_map.dia"

// RUN: %empty-directory(%t/d)
// RUN: cd %t && %swiftc_driver -disable-only-one-dependency-file -driver-print-bindings -target x86_64-apple-macosx10.9 -emit-executable -emit-module -serialize-diagnostics -emit-dependencies %/s %/S/Inputs/main.swift %/S/Inputs/lib.swift -g -o ./advanced_output_file_map.out -emit-module-path ./OutputFileMap.swiftmodule -module-name OutputFileMap -output-file-map %t/ofm.json 2>&1 | %FileCheck %/s -check-prefix=BINDINGS-DIS

// Should be no dummy files:
// RUN: test ! -e %t/d/advanced_output_file_map.d
// RUN: test ! -e %t/d/main.d
// RUN: test ! -e %t/d/lib.d


// BINDINGS-DIS: # "x86_64-apple-macosx10.9" - "swift{{(c|c-legacy-driver|-frontend)?(\.exe)?}}", inputs: ["{{.*}}/advanced_output_file_map.swift"], output: {object: "./obj/advanced_output_file_map.o", dependencies: "./d/advanced_output_file_map.d", swiftmodule: "./swiftmodule/advanced_output_file_map.swiftmodule", swiftdoc: "./swiftmodule/advanced_output_file_map_x.swiftdoc", swiftsourceinfo: "./swiftmodule{{[/\\]}}advanced_output_file_map.swiftsourceinfo", diagnostics: "./dia/advanced_output_file_map.dia"}
// BINDINGS-DIS: # "x86_64-apple-macosx10.9" - "swift{{(c|c-legacy-driver|-frontend)?(\.exe)?}}", inputs: ["{{.*}}/Inputs/main.swift"], output: {object: "./obj/main.o", dependencies: "./d/main.d", swiftmodule: "./swiftmodule/main.swiftmodule", swiftdoc: "./swiftmodule/main_x.swiftdoc", swiftsourceinfo: "./swiftmodule{{[/\\]}}main.swiftsourceinfo", diagnostics: "./dia/main.dia"}
// BINDINGS-DIS: # "x86_64-apple-macosx10.9" - "swift{{(c|c-legacy-driver|-frontend)?(\.exe)?}}", inputs: ["{{.*}}/Inputs/lib.swift"], output: {object: "./obj/lib.o", dependencies: "./d/lib.d", swiftmodule: "./swiftmodule/lib.swiftmodule", swiftdoc: "./swiftmodule/lib_x.swiftdoc", swiftsourceinfo: "./swiftmodule{{[/\\]}}lib.swiftsourceinfo", diagnostics: "./dia/lib.dia"}
// BINDINGS-DIS: # "x86_64-apple-macosx10.9" - "swift{{(c|c-legacy-driver|-frontend)?(\.exe)?}}", inputs: ["./obj/advanced_output_file_map.o", "./obj/main.o", "./obj/lib.o"], output: {swiftmodule: "./OutputFileMap.swiftmodule", swiftdoc: ".{{[/\\]}}OutputFileMap.swiftdoc", swiftsourceinfo: ".{{[/\\]}}OutputFileMap.swiftsourceinfo"}
// BINDINGS-DIS: # "x86_64-apple-macosx10.9" - "ld{{(.exe)?}}", inputs: ["./obj/advanced_output_file_map.o", "./obj/main.o", "./obj/lib.o", "./OutputFileMap.swiftmodule"], output: {image: "./advanced_output_file_map.out"}
// BINDINGS-DIS: # "x86_64-apple-macosx10.9" - "dsymutil{{(\.exe)?}}", inputs: ["./advanced_output_file_map.out"], output: {dSYM: "./advanced_output_file_map.out.dSYM"}


// With -enable-only-one-dependency-file

// RUN: cd %t && %swiftc_driver -enable-only-one-dependency-file -driver-print-output-file-map -target x86_64-apple-macosx10.9 -emit-executable -emit-module -serialize-diagnostics %/s %/S/Inputs/main.swift %/S/Inputs/lib.swift -g -o ./advanced_output_file_map.out -emit-module-path ./OutputFileMap.swiftmodule -module-name OutputFileMap -output-file-map %t/ofm.json 2>&1 | %FileCheck %/s -check-prefix=DUMPOFM-ENA


// DUMPOFM-ENA: {{.*}}/Inputs/lib.swift -> object: "./obj/lib.o"
// DUMPOFM-ENA-NEXT: {{.*}}/Inputs/lib.swift -> dependencies: "./d/lib.d"
// DUMPOFM-ENA-NEXT: {{.*}}/Inputs/lib.swift -> swiftmodule: "./swiftmodule/lib.swiftmodule"
// DUMPOFM-ENA-NEXT: {{.*}}/Inputs/lib.swift -> swiftdoc: "./swiftmodule/lib_x.swiftdoc"
// DUMPOFM-ENA-NEXT: {{.*}}/Inputs/lib.swift -> diagnostics: "./dia/lib.dia"
// DUMPOFM-ENA-NEXT: {{.*}}/Inputs/main.swift -> object: "./obj/main.o"
// DUMPOFM-ENA-NEXT: {{.*}}/Inputs/main.swift -> dependencies: "./d/main.d"
// DUMPOFM-ENA-NEXT: {{.*}}/Inputs/main.swift -> swiftmodule: "./swiftmodule/main.swiftmodule"
// DUMPOFM-ENA-NEXT: {{.*}}/Inputs/main.swift -> swiftdoc: "./swiftmodule/main_x.swiftdoc"
// DUMPOFM-ENA-NEXT: {{.*}}/Inputs/main.swift -> diagnostics: "./dia/main.dia"
// DUMPOFM-ENA-NEXT: {{.*}}/advanced_output_file_map.swift -> object: "./obj/advanced_output_file_map.o"
// DUMPOFM-ENA-NEXT: {{.*}}/advanced_output_file_map.swift -> dependencies: "./d/advanced_output_file_map.d"
// DUMPOFM-ENA-NEXT: {{.*}}/advanced_output_file_map.swift -> swiftmodule: "./swiftmodule/advanced_output_file_map.swiftmodule"
// DUMPOFM-ENA-NEXT: {{.*}}/advanced_output_file_map.swift -> swiftdoc: "./swiftmodule/advanced_output_file_map_x.swiftdoc"
// DUMPOFM-ENA-NEXT: {{.*}}/advanced_output_file_map.swift -> diagnostics: "./dia/advanced_output_file_map.dia"

// RUN: %empty-directory(%t/d)
// RUN: cd %t && %swiftc_driver -enable-only-one-dependency-file -driver-print-bindings -target x86_64-apple-macosx10.9 -emit-executable -emit-module -serialize-diagnostics -emit-dependencies %/s %/S/Inputs/main.swift %/S/Inputs/lib.swift -g -o ./advanced_output_file_map.out -emit-module-path ./OutputFileMap.swiftmodule -module-name OutputFileMap -output-file-map %t/ofm.json 2>&1 | %FileCheck %/s -check-prefix=BINDINGS-ENA


// Should be two dummy files:
// RUN: test ! -e %t/d/advanced_output_file_map.d
// RUN: test -e %t/d/main.d -a ! -s %t/d/main.d
// RUN: test -e %t/d/lib.d  -a ! -s %t/d/lib.d


// BINDINGS-ENA: # "x86_64-apple-macosx10.9" - "swift{{(c|c-legacy-driver|-frontend)?(\.exe)?}}", inputs: ["{{.*}}/advanced_output_file_map.swift"], output: {object: "./obj/advanced_output_file_map.o", dependencies: "./d/advanced_output_file_map.d", swiftmodule: "./swiftmodule/advanced_output_file_map.swiftmodule", swiftdoc: "./swiftmodule/advanced_output_file_map_x.swiftdoc", swiftsourceinfo: "./swiftmodule{{[/\\]}}advanced_output_file_map.swiftsourceinfo", diagnostics: "./dia/advanced_output_file_map.dia"}
// BINDINGS-ENA: # "x86_64-apple-macosx10.9" - "swift{{(c|c-legacy-driver|-frontend)?(\.exe)?}}", inputs: ["{{.*}}/Inputs/main.swift"], output: {object: "./obj/main.o", swiftmodule: "./swiftmodule/main.swiftmodule", swiftdoc: "./swiftmodule/main_x.swiftdoc", swiftsourceinfo: "./swiftmodule{{[/\\]}}main.swiftsourceinfo", diagnostics: "./dia/main.dia"}
// BINDINGS-ENA: # "x86_64-apple-macosx10.9" - "swift{{(c|c-legacy-driver|-frontend)?(\.exe)?}}", inputs: ["{{.*}}/Inputs/lib.swift"], output: {object: "./obj/lib.o", swiftmodule: "./swiftmodule/lib.swiftmodule", swiftdoc: "./swiftmodule/lib_x.swiftdoc", swiftsourceinfo: "./swiftmodule{{[/\\]}}lib.swiftsourceinfo", diagnostics: "./dia/lib.dia"}
// BINDINGS-ENA: # "x86_64-apple-macosx10.9" - "swift{{(c|c-legacy-driver|-frontend)?(\.exe)?}}", inputs: ["./obj/advanced_output_file_map.o", "./obj/main.o", "./obj/lib.o"], output: {swiftmodule: "./OutputFileMap.swiftmodule", swiftdoc: ".{{[/\\]}}OutputFileMap.swiftdoc", swiftsourceinfo: ".{{[/\\]}}OutputFileMap.swiftsourceinfo"}
// BINDINGS-ENA: # "x86_64-apple-macosx10.9" - "ld{{(.exe)?}}", inputs: ["./obj/advanced_output_file_map.o", "./obj/main.o", "./obj/lib.o", "./OutputFileMap.swiftmodule"], output: {image: "./advanced_output_file_map.out"}
// BINDINGS-ENA: # "x86_64-apple-macosx10.9" - "dsymutil{{(\.exe)?}}", inputs: ["./advanced_output_file_map.out"], output: {dSYM: "./advanced_output_file_map.out.dSYM"}

// Defaulting to: -disable-only-one-dependency-file

// RUN: %swiftc_driver -driver-print-output-file-map -target x86_64-apple-macosx10.9 -emit-executable -emit-module -serialize-diagnostics %/s %/S/Inputs/main.swift %/S/Inputs/lib.swift -g -o ./advanced_output_file_map.out -emit-module-path ./OutputFileMap.swiftmodule -module-name OutputFileMap -output-file-map %t/ofm.json 2>&1 | %FileCheck %/s -check-prefix=DUMPOFM-DIS


// RUN: %empty-directory(%t/d)
// RUN: %swiftc_driver -driver-print-bindings -target x86_64-apple-macosx10.9 -emit-executable -emit-module -serialize-diagnostics -emit-dependencies %/s %/S/Inputs/main.swift %/S/Inputs/lib.swift -g -o ./advanced_output_file_map.out -emit-module-path ./OutputFileMap.swiftmodule -module-name OutputFileMap -output-file-map %t/ofm.json 2>&1 | %FileCheck %/s -check-prefix=BINDINGS-DIS

// Should be no dummy files:
// RUN: test ! -e %t/d/advanced_output_file_map.d
// RUN: test ! -e %t/d/main.d -a ! -s %t/d/main.d
// RUN: test ! -e %t/d/lib.d  -a ! -s %t/d/lib.d

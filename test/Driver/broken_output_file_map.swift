// RUN: echo "{\"%/s\": {\"object\": \"/build/broken_output_file_map.o\", \"completely-bogus-type\": \"\"}}" > %t.json

// RUN: %swiftc_driver -driver-print-output-file-map -target x86_64-apple-macosx10.9 -emit-executable %s %/S/Inputs/main.swift %/S/Inputs/lib.swift -o /build/basic_output_file_map.out -module-name OutputFileMap -output-file-map %t.json 2>&1 | %FileCheck %s

// CHECK: {{.*}}/broken_output_file_map.swift -> object: "/build/broken_output_file_map.o"

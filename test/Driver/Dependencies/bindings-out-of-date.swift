// RUN: rm -rf %t && mkdir %t
// RUN: echo "{\"%t/source.swift\": {\"object\": \"%t/source.o\", \"swift-dependencies\": \"%t/source.swiftdeps\"}, \"\": {\"swift-dependencies\": \"%t/main~buildrecord.swiftdeps\"}}" > %t.json

// RUN: touch -t 201401240005 %t/source.swift
// RUN: %swiftc_driver -driver-print-bindings %t/source.swift -incremental -output-file-map %t.json 2>&1 | FileCheck %s -check-prefix=MUST-EXEC -check-prefix=CHECK

// RUN: touch -t 201401240006 %t/source.o
// RUN: %swiftc_driver -driver-print-bindings %t/source.swift -incremental -output-file-map %t.json 2>&1 | FileCheck %s -check-prefix=MAY-EXEC -check-prefix=CHECK

// RUN: touch -t 201401240004 %t/source.o
// RUN: %swiftc_driver -driver-print-bindings %t/source.swift -incremental -output-file-map %t.json 2>&1 | FileCheck %s -check-prefix=MUST-EXEC -check-prefix=CHECK

// CHECK-NOT: warning
// MUST-EXEC: inputs: ["{{.*}}/source.swift"], output: {{[{].*[}]$}}
// MAY-EXEC: inputs: ["{{.*}}/source.swift"], output: {{[{].*[}]}}, condition: check-dependencies{{$}}

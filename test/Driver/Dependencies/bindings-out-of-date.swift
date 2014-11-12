// RUN: rm -rf %t && mkdir %t
// RUN: echo "{\"%t/source.swift\": {\"object\": \"%t/source.o\", \"swift-dependencies\": \"%t/source.swiftdeps\"}}" > %t.json

// RUN: touch -t 201401240005 %t/source.swift
// RUN: %swiftc_driver -driver-print-bindings %t/source.swift -emit-reference-dependencies -output-file-map %t.json 2>&1 | FileCheck %s -check-prefix=MUST-EXEC

// RUN: touch -t 201401240006 %t/source.o
// RUN: %swiftc_driver -driver-print-bindings %t/source.swift -emit-reference-dependencies -output-file-map %t.json 2>&1 | FileCheck %s -check-prefix=MAY-EXEC

// RUN: touch -t 201401240004 %t/source.o
// RUN: %swiftc_driver -driver-print-bindings %t/source.swift -emit-reference-dependencies -output-file-map %t.json 2>&1 | FileCheck %s -check-prefix=MUST-EXEC

// MUST-EXEC: inputs: ["{{.*}}/source.swift"], output: {{[{].*[}]$}}
// MAY-EXEC: inputs: ["{{.*}}/source.swift"], output: {{[{].*[}]}}, condition: check-dependencies{{$}}

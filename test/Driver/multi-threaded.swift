// RUN: rm -rf %t && mkdir %t
// RUN: %target-swiftc_driver -driver-print-jobs -module-name=test -wmo -num-threads 4 %S/Inputs/main.swift %s -emit-module -o test.swiftmodule | FileCheck -check-prefix=MODULE %s
// RUN: echo "{\"%s\": {\"assembly\": \"/build/multi-threaded.s\"}, \"%S/Inputs/main.swift\": {\"assembly\": \"/build/main.s\"}}" > %t/ofms.json
// RUN: %target-swiftc_driver -driver-print-jobs -module-name=test -wmo -num-threads 4 %S/Inputs/main.swift %s -output-file-map %t/ofms.json -S | FileCheck -check-prefix=ASSEMBLY %s
// RUN: %target-swiftc_driver -driver-print-jobs -module-name=test -wmo -num-threads 4 %S/Inputs/main.swift %s -c | FileCheck -check-prefix=OBJECT %s
// RUN: env TMPDIR=/tmp %swiftc_driver -driver-print-jobs -module-name=test -wmo -num-threads 4 %S/Inputs/main.swift %s -o a.out | FileCheck -check-prefix=EXEC %s
// RUN: echo "{\"%s\": {\"object\": \"%t/multi-threaded.o\"}, \"%S/Inputs/main.swift\": {\"object\": \"%t/main.o\"}}" > %t/ofmo.json
// RUN: %target-swiftc_driver -module-name=ThisModule -wmo -num-threads 4 %S/Inputs/main.swift %s  -emit-dependencies -output-file-map %t/ofmo.json -c
// RUN: cat %t/*.d | FileCheck -check-prefix=DEPENDENCIES %s

// MODULE: -frontend
// MODULE-DAG: -num-threads 4
// MODULE-DAG: {{[^ ]*}}/Inputs/main.swift {{[^ ]*}}/multi-threaded.swift 
// MODULE-DAG: -o test.swiftmodule
// MODULE-NOT: ld

// ASSEMBLY: -frontend
// ASSEMBLY-DAG: -num-threads 4
// ASSEMBLY-DAG: {{[^ ]*}}/Inputs/main.swift {{[^ ]*}}/multi-threaded.swift 
// ASSEMBLY-DAG: -o /build/main.s -o /build/multi-threaded.s
// ASSEMBLY-NOT: ld

// OBJECT: -frontend
// OBJECT-DAG: -num-threads 4
// OBJECT-DAG: {{[^ ]*}}/Inputs/main.swift {{[^ ]*}}/multi-threaded.swift 
// OBJECT-DAG: -o main.o -o multi-threaded.o 
// OBJECT-NOT: ld

// EXEC: -frontend
// EXEC-DAG: -num-threads 4
// EXEC-DAG: {{[^ ]*}}/Inputs/main.swift {{[^ ]*}}/multi-threaded.swift 
// EXEC-DAG:  -o /tmp/main{{[^ ]*}}.o -o /tmp/multi-threaded{{[^ ]*}}.o
// EXEC: ld
// EXEC: /tmp/main{{[^ ]*}}.o /tmp/multi-threaded{{[^ ]*}}.o

// DEPENDENCIES-DAG: {{.*}}/multi-threaded.o : {{.*}}/Inputs/main.swift {{.*}}/multi-threaded.swift
// DEPENDENCIES-DAG: {{.*}}/main.o : {{.*}}/Inputs/main.swift {{.*}}/multi-threaded.swift

func libraryFunction() {}


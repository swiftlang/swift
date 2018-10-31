// RUN: %empty-directory(%t)
// RUN: %target-swiftc_driver -driver-print-jobs -module-name=ThisModule -wmo -num-threads 4 %S/Inputs/main.swift %s -emit-module -o test.swiftmodule | %FileCheck -check-prefix=MODULE %s
// RUN: echo "{\"%/s\": {\"assembly\": \"/build/multi-threaded.s\"}, \"%/S/Inputs/main.swift\": {\"assembly\": \"/build/main.s\"}}" > %t/ofms.json
// RUN: %target-swiftc_driver -driver-print-jobs -module-name=ThisModule -wmo -num-threads 4 %/S/Inputs/main.swift %/s -output-file-map %t/ofms.json -S | %FileCheck -check-prefix=ASSEMBLY %s
// RUN: %target-swiftc_driver -driver-print-jobs -module-name=ThisModule -wmo -num-threads 4 %S/Inputs/main.swift %s -c | %FileCheck -check-prefix=OBJECT %s
// RUN: cd %t && %target-swiftc_driver -parseable-output -module-name=ThisModule -wmo -num-threads 4 %S/Inputs/main.swift %s -c 2> %t/parseable-output
// RUN: cat %t/parseable-output | %FileCheck -check-prefix=PARSEABLE %s
// RUN: cd %t && env TMPDIR=/tmp %swiftc_driver -driver-print-jobs -module-name=ThisModule -wmo -num-threads 4 %S/Inputs/main.swift %s -o a.out | %FileCheck -check-prefix=EXEC %s
// RUN: echo "{\"%/s\": {\"llvm-bc\": \"%/t/multi-threaded.bc\", \"object\": \"%/t/multi-threaded.o\"}, \"%/S/Inputs/main.swift\": {\"llvm-bc\": \"%/t/main.bc\", \"object\": \"%/t/main.o\"}}" > %t/ofmo.json
// RUN: %target-swiftc_driver -module-name=ThisModule -wmo -num-threads 4 %S/Inputs/main.swift %s  -emit-dependencies -output-file-map %t/ofmo.json -c
// RUN: cat %t/*.d | %FileCheck -check-prefix=DEPENDENCIES %s

// Check if -embed-bitcode works
// RUN: %target-swiftc_driver -driver-print-jobs -module-name=ThisModule -embed-bitcode -wmo -num-threads 4 %/S/Inputs/main.swift %/s -output-file-map %t/ofmo.json -c | %FileCheck -check-prefix=BITCODE %s

// Check if -embed-bitcode works with -parseable-output
// RUN: %target-swiftc_driver -parseable-output -module-name=ThisModule -embed-bitcode -wmo -num-threads 4 %/S/Inputs/main.swift %/s -output-file-map %t/ofmo.json -c 2> %t/parseable2
// RUN: cat %t/parseable2 | %FileCheck -check-prefix=PARSEABLE2 %s

// Check if linking works with -parseable-output
// RUN: cd %t && %target-swiftc_driver -parseable-output -module-name=ThisModule -wmo -num-threads 4 %/S/Inputs/main.swift %/s -output-file-map %t/ofmo.json -o a.out 2> %t/parseable3
// RUN: cat %t/parseable3 | %FileCheck -check-prefix=PARSEABLE3 %s

// MODULE: -frontend
// MODULE-DAG: -num-threads 4
// MODULE-DAG: {{[^ ]*[/\\]}}Inputs{{/|\\\\}}main.swift{{"?}} {{[^ ]*[/\\]}}multi-threaded.swift 
// MODULE-DAG: -o test.swiftmodule
// MODULE-NOT: ld

// ASSEMBLY: -frontend
// ASSEMBLY-DAG: -num-threads 4
// ASSEMBLY-DAG: {{[^ ]*[/\\]}}Inputs{{/|\\\\}}main.swift{{"?}} {{[^ ]*[/\\]}}multi-threaded.swift 
// ASSEMBLY-DAG: -o /build/main.s -o /build/multi-threaded.s
// ASSEMBLY-NOT: ld

// OBJECT: -frontend
// OBJECT-DAG: -num-threads 4
// OBJECT-DAG: {{[^ ]*[/\\]}}Inputs{{/|\\\\}}main.swift{{"?}} {{[^ ]*[/\\]}}multi-threaded.swift 
// OBJECT-DAG: -o main.o -o multi-threaded.o 
// OBJECT-NOT: ld

// BITCODE: -frontend
// BITCODE-DAG: -num-threads 4
// BITCODE-DAG: {{[^ ]*[/\\]}}Inputs{{/|\\\\}}main.swift{{"?}} {{[^ ]*[/\\]}}multi-threaded.swift 
// BITCODE-DAG: -o {{.*[/\\]}}main.bc -o {{.*[/\\]}}multi-threaded.bc
// BITCODE-DAG: -frontend -c -primary-file {{.*[/\\]}}main.bc {{.*}} -o {{[^ ]*}}main.o
// BITCODE-DAG: -frontend -c -primary-file {{.*[/\\]}}multi-threaded.bc {{.*}} -o {{[^ ]*}}multi-threaded.o
// BITCODE-NOT: ld

// PARSEABLE: "outputs": [
// PARSEABLE: "path": "main.o"
// PARSEABLE: "path": "multi-threaded.o"

// EXEC: -frontend
// EXEC-DAG: -num-threads 4
// EXEC-DAG: {{[^ ]*[/\\]}}Inputs{{/|\\\\}}main.swift{{"?}} {{[^ ]*[/\\]}}multi-threaded.swift 
// EXEC-DAG:  -o {{.*te?mp.*[/\\]}}main{{[^ ]*}}.o{{"?}} -o {{.*te?mp.*[/\\]}}multi-threaded{{[^ ]*}}.o
// EXEC: ld
// EXEC:  {{.*te?mp.*[/\\]}}main{{[^ ]*}}.o{{"?}} {{.*te?mp.*[/\\]}}multi-threaded{{[^ ]*}}.o

// DEPENDENCIES-DAG: {{.*}}multi-threaded.o : {{.*[/\\]}}multi-threaded.swift {{.*[/\\]}}Inputs{{[/\\]}}main.swift
// DEPENDENCIES-DAG: {{.*}}main.o : {{.*[/\\]}}multi-threaded.swift {{.*[/\\]}}Inputs{{[/\\]}}main.swift

// PARSEABLE2: "name": "compile"
// PARSEABLE2: "outputs": [
// PARSEABLE2: "path": "{{.*[/\\]}}main.bc"
// PARSEABLE2: "path": "{{.*[/\\]}}multi-threaded.bc"
// PARSEABLE2: "name": "backend"
// PARSEABLE2: "inputs": [
// PARSEABLE2:   "{{.*[/\\]}}main.bc"
// PARSEABLE2: "outputs": [
// PARSEABLE2:   "path": "{{.*[/\\]}}main.o"
// PARSEABLE2: "name": "backend"
// PARSEABLE2: "inputs": [
// PARSEABLE2:   "{{.*[/\\]}}multi-threaded.bc"
// PARSEABLE2: "outputs": [
// PARSEABLE2:   "path": "{{.*[/\\]}}multi-threaded.o"

// PARSEABLE3: "name": "compile"
// PARSEABLE3: "outputs": [
// PARSEABLE3:   "path": "{{.*}}main.o"
// PARSEABLE3:   "path": "{{.*}}multi-threaded.o"
// PARSEABLE3: "name": "link"
// PARSEABLE3: "inputs": [
// PARSEABLE3-NEXT:   "{{.*}}main.o"
// PARSEABLE3-NEXT:   "{{.*}}multi-threaded.o"
// PARSEABLE3: "outputs": [
// PARSEABLE3:   "path": "a.out"

func libraryFunction() {}


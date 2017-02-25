// RUN: rm -rf %t && mkdir -p %t

// RUN: touch %t/module1.swiftmodule


// Emit named module

// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.10 -c -emit-module-path %t/module1.swiftmodule -module-name module1 -parse-as-library %s 2>&1 | %FileCheck %s -check-prefixes=COMPILE-FILE,EMIT-MODULE

// RUN: %swiftc_driver -driver-print-jobs -target x86_64-unknown-linux-gnu -c -emit-module-path %t/module1.swiftmodule -module-name module1 -parse-as-library %s 2>&1 | %FileCheck %s -check-prefixes=COMPILE-FILE,EMIT-MODULE

// COMPILE-FILE: bin/swift
// COMPILE-FILE: -c
// COMPILE-FILE: -primary-file {{[^ ]+}}/multiple_swiftmodule_inputs.swift

// EMIT-MODULE: -emit-module-doc-path {{[^ ]+}}/multiple_swiftmodule_inputs-{{[^ ]+}}.swiftdoc
// EMIT-MODULE: -parse-as-library
// EMIT-MODULE: -module-name module1
// EMIT-MODULE: -emit-module-path {{[^ ]+}}/multiple_swiftmodule_inputs-{{[^ ]+}}.swiftmodule
// EMIT-MODULE: -o {{[^ ]+}}/multiple_swiftmodule_inputs-{{[^ ]+}}.o
// EMIT-MODULE: bin/swift
// EMIT-MODULE: -emit-module 
// EMIT-MODULE: {{[^ ]+}}/multiple_swiftmodule_inputs-{{[^ ]+}}.swiftmodule
// EMIT-MODULE: -parse-as-library
// EMIT-MODULE: -emit-module-doc-path {{[^ ]+}}/module1.swiftdoc
// EMIT-MODULE: -module-name module1
// EMIT-MODULE: -o {{[^ ]+}}/module1.swiftmodule


// Compile and link with swiftmodule

// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.10 %s %t/module1.swiftmodule 2>&1 | %FileCheck %s -check-prefixes=COMPILE-FILE,EMIT-OBJECT,DARWIN-LINK-OBJECT-AND-MODULE

// EMIT-OBJECT: -module-name main
// EMIT-OBJECT: -o {{[^ ]+}}/multiple_swiftmodule_inputs-{{[^ ]+}}.o

// DARWIN-LINK-OBJECT-AND-MODULE: bin/ld {{[^ ]+}}/multiple_swiftmodule_inputs{{(-[^ ]+)?}}.o
// DARWIN-LINK-OBJECT-AND-MODULE: -add_ast_path {{[^ ]+}}/module1.swiftmodule
// DARWIN-LINK-OBJECT-AND-MODULE: -o main

// RUN: %swiftc_driver -driver-print-jobs -target x86_64-unknown-linux-gnu %s %t/module1.swiftmodule 2>&1 | %FileCheck %s -check-prefixes=COMPILE-FILE,LINUX-AUTOLINK-EXTRACT,LINUX-LINK-OBJECT-AND-MODULE

// LINUX-AUTOLINK-EXTRACT: bin/swift-autolink-extract
// LINUX-AUTOLINK-EXTRACT: {{[^ ]+}}/multiple_swiftmodule_inputs{{-?[^ ]+}}.o
// LINUX-AUTOLINK-EXTRACT: -o {{[^ ]+}}/multiple_swiftmodule_inputs-{{[^ ]+}}.autolink

// LINUX-LINK-OBJECT-AND-MODULE: bin/clang++ -fuse-ld
// LINUX-LINK-OBJECT-AND-MODULE: {{[^ ]+}}/multiple_swiftmodule_inputs{{-?[^ ]+}}.o
// LINUX-LINK-OBJECT-AND-MODULE: {{[^ ]+}}/module1.swiftmodule
// LINUX-LINK-OBJECT-AND-MODULE: {{[^ ]+}}/multiple_swiftmodule_inputs-{{[^ ]+}}.autolink
// LINUX-LINK-OBJECT-AND-MODULE: -o main


// Link objects

// RUN: touch %t/multiple_swiftmodule_inputs.o

// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.10 %t/multiple_swiftmodule_inputs.o %t/module1.swiftmodule 2>&1 | %FileCheck %s -check-prefix=DARWIN-LINK-OBJECT-AND-MODULE

// RUN: %swiftc_driver -driver-print-jobs -target x86_64-unknown-linux-gnu %t/multiple_swiftmodule_inputs.o %t/module1.swiftmodule 2>&1 | %FileCheck %s -check-prefix=LINUX-LINK-OBJECT-AND-MODULE


// Compile with -g and link with swiftmodule

// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.10 -g %s %t/module1.swiftmodule 2>&1 | %FileCheck %s -check-prefixes=COMPILE-FILE,EMIT-OBJECT-DEBUG,EMIT-MODULE-DEBUG,DARWIN-OBJECT-AND-MODULE-DEBUG

// EMIT-OBJECT-DEBUG: -g
// EMIT-OBJECT-DEBUG: -emit-module-doc-path {{[^ ]+}}/multiple_swiftmodule_inputs-{{[^ ]+}}.swiftdoc
// EMIT-OBJECT-DEBUG: -module-name main
// EMIT-OBJECT-DEBUG: -emit-module-path {{[^ ]+}}/multiple_swiftmodule_inputs-{{[^ ]+}}.swiftmodule
// EMIT-OBJECT-DEBUG: -o {{[^ ]+}}/multiple_swiftmodule_inputs-{{[^ ]+}}.o

// EMIT-MODULE-DEBUG: bin/swift
// EMIT-MODULE-DEBUG: -emit-module
// EMIT-MODULE-DEBUG: {{[^ ]+}}/multiple_swiftmodule_inputs-{{[^ ]+}}.swiftmodule
// EMIT-MODULE-DEBUG: {{[^ ]+}}/module1.swiftmodule
// EMIT-MODULE-DEBUG: -parse-as-library
// EMIT-MODULE-DEBUG: -g
// EMIT-MODULE-DEBUG: -emit-module-doc-path {{[^ ]+}}/main-{{[^ ]+}}.swiftdoc
// EMIT-MODULE-DEBUG: -module-name main
// EMIT-MODULE-DEBUG: -o {{[^ ]+}}/main-{{[^ ]+}}.swiftmodule

// DARWIN-OBJECT-AND-MODULE-DEBUG: bin/ld {{[^ ]+}}/multiple_swiftmodule_inputs-{{[^ ]+}}.o
// DARWIN-OBJECT-AND-MODULE-DEBUG: -add_ast_path {{[^ ]+}}/main-{{[^ ]+}}.swiftmodule
// DARWIN-OBJECT-AND-MODULE-DEBUG: -o main


// RUN: %swiftc_driver -driver-print-jobs -target x86_64-unknown-linux-gnu -g %s %t/module1.swiftmodule 2>&1 | %FileCheck %s -check-prefixes=COMPILE-FILE,LINUX-AUTOLINK-EXTRACT,EMIT-MODULE-DEBUG,LINUX-MODULEWRAP,LINUX-LINK-OBJECTS

// LINUX-MODULEWRAP: bin/swift -modulewrap
// LINUX-MODULEWRAP: {{[^ ]+}}/main-{{[^ ]+}}.swiftmodule
// LINUX-MODULEWRAP: -o {{[^ ]+}}/module1-{{.*}}.o

// LINUX-LINK-OBJECTS: bin/clang++ -fuse-ld
// LINUX-LINK-OBJECTS: {{[^ ]+}}/multiple_swiftmodule_inputs-{{[^ ]+}}.o
// LINUX-LINK-OBJECTS: {{[^ ]+}}/module1-{{[^ ]+}}.o
// LINUX-LINK-OBJECTS: -o main


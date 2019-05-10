// RUN: %empty-directory(%t)

// RUN: cd %t && %swiftc_driver -driver-print-jobs -working-directory %/S/Inputs -c main.swift | %FileCheck %s -check-prefix=INPUT
// RUN: cd %t && %swiftc_driver -driver-print-jobs -working-directory %/S/Inputs -c %/S/Inputs/main.swift | %FileCheck %s -check-prefix=INPUT
// INPUT: SOURCE_DIR/test/Driver/Inputs{{/|\\\\}}main.swift

// Relative -working-directory...
// RUN: cd %S && %swiftc_driver -driver-print-jobs -working-directory Inputs -c %/S/Inputs/main.swift | %FileCheck %s -check-prefix=INPUT

// -working-directory=
// RUN: cd %t && %swiftc_driver -driver-print-jobs -working-directory=%S/Inputs -c %/S/Inputs/main.swift | %FileCheck %s -check-prefix=INPUT

// In another driver mode.
// RUN: cd %t && %swift_driver -driver-print-jobs -working-directory %/S/Inputs -F. | %FileCheck %s -check-prefix=REPL
// RUN: cd %t && %swift_driver -driver-print-jobs -deprecated-integrated-repl -working-directory %/S/Inputs -F. | %FileCheck %s -check-prefix=REPL
// REPL: -F {{\\?"?}}SOURCE_DIR/test/Driver/Inputs{{(\\\\(\\\\)?)|/}}.

// RUN: cd %t && %swiftc_driver -driver-print-jobs -working-directory=%/S/Inputs -c -module-name m main.swift lib.swift | %FileCheck %s -check-prefix=MULTI_INPUT
// MULTI_INPUT: SOURCE_DIR/test/Driver/Inputs{{/|\\\\}}main.swift
// MULTI_INPUT-SAME: SOURCE_DIR/test/Driver/Inputs{{/|\\\\}}lib.swift

// Using --
// RUN: cd %t && %swiftc_driver -driver-print-jobs -working-directory=%/S/Inputs -c -module-name m -- main.swift lib.swift | %FileCheck %s -check-prefix=MULTI_INPUT

// RUN: cd %t && %swiftc_driver -driver-print-jobs -working-directory %/S/Inputs -c %/s -F . | %FileCheck %s -check-prefix=SEARCH_PATH
// RUN: cd %t && %swiftc_driver -driver-print-jobs -working-directory %/S/Inputs -c %/s -F. | %FileCheck %s -check-prefix=SEARCH_PATH
// RUN: cd %t && %swiftc_driver -driver-print-jobs -working-directory %/S/Inputs -c %/s -F=. | %FileCheck %s -check-prefix=SEARCH_PATH
// SEARCH_PATH: -F {{"?}}SOURCE_DIR/test/Driver/Inputs{{/|\\\\}}.

// RUN: cd %t && %swiftc_driver -driver-print-jobs -working-directory %/S/Inputs -emit-executable %/s -L . | %FileCheck %s -check-prefix=L_PATH
// RUN: cd %t && %swiftc_driver -driver-print-jobs -working-directory %/S/Inputs -emit-executable %/s -L. | %FileCheck %s -check-prefix=L_PATH
// RUN: cd %t && %swiftc_driver -driver-print-jobs -working-directory %/S/Inputs -emit-executable %/s -L=. | %FileCheck %s -check-prefix=L_PATH
// L_PATH: -L {{"?}}SOURCE_DIR/test/Driver/Inputs{{/|\\\\}}.

// RUN: cd %t && %swiftc_driver -driver-print-jobs -working-directory %/S/Inputs -c %/s -disable-bridging-pch -import-objc-header bridging-header.h | %FileCheck %s -check-prefix=OBJC_HEADER1
// OBJC_HEADER1: -import-objc-header {{"?}}SOURCE_DIR/test/Driver/Inputs{{/|\\\\}}bridging-header.h

// RUN: cd %t && %swiftc_driver -driver-print-jobs -working-directory %/S/Inputs -c %/s -enable-bridging-pch -import-objc-header bridging-header.h | %FileCheck %s -check-prefix=OBJC_HEADER2
// OBJC_HEADER2: SOURCE_DIR/test/Driver/Inputs{{/|\\\\}}bridging-header.h{{"? .*}}-emit-pch

// RUN: cd %t && %swiftc_driver -driver-print-jobs -working-directory %/S/Inputs -c %/s -o main.o | %FileCheck %s -check-prefix=OUTPUT_OBJ
// RUN: cd %t && %swiftc_driver -driver-print-jobs -working-directory %/S/Inputs -c %/s -o %/S/Inputs/main.o | %FileCheck %s -check-prefix=OUTPUT_OBJ
// OUTPUT_OBJ: -o {{"?}}SOURCE_DIR/test/Driver/Inputs{{/|\\\\}}main.o

// RUN: cd %t && %swiftc_driver -driver-print-jobs -working-directory %/S/Inputs -c %/s -module-cache-path mcp | %FileCheck %s -check-prefix=ARG_IS_PATH
// RUN: cd %t && %swiftc_driver -driver-print-jobs -working-directory %/S/Inputs -c %/s -module-cache-path %/S/Inputs/mcp | %FileCheck %s -check-prefix=ARG_IS_PATH
// ARG_IS_PATH: -module-cache-path {{"?}}SOURCE_DIR/test/Driver/Inputs{{\\\\|/}}mcp

// RUN: cd %t && %swiftc_driver -driver-print-jobs -working-directory %/S/Inputs -emit-executable %/s -o main | %FileCheck %s -check-prefix=OUTPUT_EXE
// RUN: cd %t && %swiftc_driver -driver-print-jobs -working-directory %/S/Inputs -emit-executable %/s -o %/S/Inputs/main | %FileCheck %s -check-prefix=OUTPUT_EXE
// OUTPUT_EXE: -o {{"?}}SOURCE_DIR/test/Driver/Inputs{{\\\\|/}}main

// RUN: cd %t && %swiftc_driver -driver-print-jobs -working-directory %/S/Inputs -emit-ir %/s -o - | %FileCheck %s -check-prefix=OUTPUT_STDOUT
// OUTPUT_STDOUT: -o -

// RUN: echo "{\"main.swift\": {\"object\": \"main-modified.o\"}}" > %t/ofmo.json
// RUN: cd %t && %swiftc_driver -driver-print-jobs -working-directory %/S/Inputs -c main.swift -output-file-map %t/ofmo.json | %FileCheck %s -check-prefix=OUTPUT_FILE_MAP_1
// OUTPUT_FILE_MAP_1: -o {{"?}}SOURCE_DIR/test/Driver/Inputs{{\\\\|/}}main-modified.o

// -output-file-map itself
// RUN: echo "{\"main.swift\": {\"object\": \"main-modified.o\"}}" > %t/ofmo2.json
// RUN: touch %t/main.swift
// RUN: cd %S && %swiftc_driver -driver-print-jobs -working-directory %/t -c main.swift -output-file-map ofmo2.json | %FileCheck %s -check-prefix=OUTPUT_FILE_MAP_2
// -output-file-map= is an alias for -output-file-map
// RUN: cd %S && %swiftc_driver -driver-print-jobs -working-directory %/t -c main.swift -output-file-map=ofmo2.json | %FileCheck %s -check-prefix=OUTPUT_FILE_MAP_2
// OUTPUT_FILE_MAP_2: BUILD_DIR{{.*}}main-modified.o

// RUN: %empty-directory(%t/sub)
// RUN: echo "{\"\": {\"swift-dependencies\": \"br.swiftdeps\"}}" > %t/sub/ofmo.json
// RUN: touch %t/sub/a.swift %t/sub/b.swift
// RUN: cd %t && %swiftc_driver -incremental -working-directory %/t/sub -emit-dependencies -c -module-name ab a.swift b.swift -output-file-map=%/t/sub/ofmo.json
// RUN: ls %t/sub/br.swiftdeps

// RUN: cd %t && %swiftc_driver -driver-print-jobs -working-directory %/S/Inputs -Xcc -working-directory -Xcc %/t -c %/s | %FileCheck %s -check-prefix=CLANG
// CLANG: -Xcc -working-directory -Xcc SOURCE_DIR
// CLANG-SAME: -Xcc -working-directory -Xcc BUILD_DIR

// RUN: cd %t && %swiftc_driver -driver-print-jobs -working-directory %/S/Inputs -c main.swift | %FileCheck %s -check-prefix=OUTPUT_IMPLICIT_OBJ
// OUTPUT_IMPLICIT_OBJ: -o {{"?}}SOURCE_DIR/test/Driver/Inputs{{\\\\|/}}main.o

// RUN: cd %t && %swiftc_driver -driver-print-jobs -working-directory %/S/Inputs -emit-module main.swift | %FileCheck %s -check-prefix=OUTPUT_IMPLICIT_MODULE
// OUTPUT_IMPLICIT_MODULE: -emit-module-doc-path {{"?}}SOURCE_DIR/test/Driver/Inputs{{\\\\|/}}main.swiftdoc
// OUTPUT_IMPLICIT_MODULE-SAME: -o {{"?}}SOURCE_DIR/test/Driver/Inputs{{\\\\|/}}main.swiftmodule

// RUN: cd %t && %swiftc_driver -driver-print-jobs -working-directory %/S/Inputs -emit-executable main.swift | %FileCheck %s -check-prefix=OUTPUT_IMPLICIT_EXE
// OUTPUT_IMPLICIT_EXE: -o {{"?}}SOURCE_DIR/test/Driver/Inputs{{\\\\|/}}main

// RUN: cd %t && %swiftc_driver -driver-print-jobs -working-directory %/S/Inputs -emit-module -emit-executable main.swift | %FileCheck %s -check-prefix=OUTPUT_IMPLICIT_EXE_AND_MODULE
// OUTPUT_IMPLICIT_EXE_AND_MODULE: -emit-module-doc-path {{"?}}SOURCE_DIR/test/Driver/Inputs{{\\\\|/}}main.swiftdoc
// OUTPUT_IMPLICIT_EXE_AND_MODULE-SAME: -o {{"?}}SOURCE_DIR/test/Driver/Inputs{{\\\\|/}}main.swiftmodule
// OUTPUT_IMPLICIT_EXE_AND_MODULE: -o {{"?}}SOURCE_DIR/test/Driver/Inputs{{\\\\|/}}main

// RUN: cd %t && %swiftc_driver -driver-print-jobs -working-directory %/S/Inputs -emit-module -emit-executable main.swift -o not_main | %FileCheck %s -check-prefix=OUTPUT_IMPLICIT_EXPLICIT
// OUTPUT_IMPLICIT_EXPLICIT: -emit-module-doc-path {{"?}}SOURCE_DIR/test/Driver/Inputs{{\\\\|/}}not_main.swiftdoc
// OUTPUT_IMPLICIT_EXPLICIT-SAME: -o {{"?}}SOURCE_DIR/test/Driver/Inputs{{\\\\|/}}not_main.swiftmodule
// OUTPUT_IMPLICIT_EXPLICIT: -o {{"?}}SOURCE_DIR/test/Driver/Inputs{{\\\\|/}}not_main

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test \
// RUN:   -module-cache-path %t/clang-module-cache -I %t \
// RUN:   -disable-implicit-string-processing-module-import \
// RUN:   -disable-implicit-concurrency-module-import \
// RUN:   %t/main.swift -o %t/deps.json -swift-version 5 \
// RUN:   -cache-compile-job -cas-path %t/cas

// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json \
// RUN:   clang:CASAPINotes clangIncludeTree > %t/CASAPINotes.tree
// RUN: clang-cas-test --cas %t/cas --print-include-tree \
// RUN:   @%t/CASAPINotes.tree | %FileCheck %s --check-prefix=INCLUDE-TREE
// RUN: rm %t/CASAPINotes.apinotes

// RUN: %{python} %S/../../utils/swift-build-modules.py --cas %t/cas \
// RUN:   %swift_frontend_plain %t/deps.json -o %t/Test.cmd
// RUN: %target-swift-frontend-plain -typecheck -module-name Test \
// RUN:   -cache-compile-job -cas-path %t/cas \
// RUN:   -disable-implicit-string-processing-module-import \
// RUN:   -disable-implicit-concurrency-module-import \
// RUN:   %t/main.swift @%t/Test.cmd

// INCLUDE-TREE: APINotes:
// INCLUDE-TREE: Name: CASAPINotes
// INCLUDE-TREE: Name: TYPED_CONSTANT
// INCLUDE-TREE: Type: DWORD

//--- module.modulemap
module CASAPINotes {
  header "CASAPINotes.h"
  export *
}

//--- CASAPINotes.h
typedef unsigned int DWORD;
#define TYPED_CONSTANT 2

//--- CASAPINotes.apinotes
Name: CASAPINotes
Globals:
  - Name: TYPED_CONSTANT
    Type: DWORD

//--- main.swift
import CASAPINotes

let _: DWORD = TYPED_CONSTANT


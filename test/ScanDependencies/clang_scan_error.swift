// REQUIRES: objc_interop
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test %t/main.swift -import-objc-header %t/bridging.h \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -o %t/deps.json -I %t -swift-version 5 2>&1 | %FileCheck %s

// CHECK: error: Clang dependency scanner failure: failed to scan bridging header dependencies

//--- bridging.h
#include "do-not-exist.h"

//--- main.swift
func test() {}

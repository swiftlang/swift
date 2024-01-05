// rdar://119964830 Temporarily disabling in Linux
// UNSUPPORTED: OS=linux-gnu

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache %t/main.swift -o %t/deps.json -I %t/include -swift-version 4 -cache-compile-job -cas-path %t/cas
// RUN: %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:DotDot > %t/DotDot.cmd
// RUN: %swift_frontend_plain @%t/DotDot.cmd

// RUN: %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:SwiftShims > %t/SwiftShims.cmd
// RUN: %swift_frontend_plain @%t/SwiftShims.cmd

// RUN: %S/Inputs/BuildCommandExtractor.py %t/deps.json Swift > %t/Swift.cmd
// RUN: %swift_frontend_plain @%t/Swift.cmd

/// Test that if there are non-existing module-map file passed through -Xcc, this still compiles.
// RUN: %swift_frontend_plain @%t/Swift.cmd  -Xcc -fmodule-map-file=%t/do-not-exist.modulemap

//--- main.swift
import DotDot

//--- include/module.modulemap
module DotDot [extern_c] {
  header "../dotdot.h"
  export *
}

//--- dotdot.h
void test(void);

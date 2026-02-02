// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)
// RUN: split-file %s %t

// REQUIRES: OS=macosx || OS=maccatalyst

// First, test that the swift interface with an invalid os version behaves fine.
// RUN: %target-swift-typecheck-module-from-interface(%t/Modules/Simple.swiftmodule/arm64-apple-macos.swiftinterface) -module-name Simple 

// Next, build transitive dependencies in zippered mode.
// RUN: %target-swift-frontend -module-name input %t/input.swift -target arm64-apple-macosx50.1 -target-variant arm64-apple-ios50.1-macabi -I%t/Modules -scan-dependencies -module-cache-path %t/module-cache-path -o %t/deps.json 2>&1 | Filecheck  --allow-empty --implicit-check-not warning: --implicit-check-not error: %s
// RUN: %validate-json %t/deps.json | %FileCheck %s --check-prefix=DEPS

DEPS-NOT:   "arm64-apple-macos16.4"
DEPS-NOT:   "arm64-apple-ios22.0-macabi"
DEPS:       "arm64-apple-macos26.4"

//--- Modules/Simple.swiftmodule/arm64-apple-macos.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -target arm64-apple-macos16.4 
public struct S {
}

//--- Modules/Simple.swiftmodule/arm64-apple-ios-macabi.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -target arm64-apple-ios22.0-macabi
public struct S {
}

//--- Modules/module.modulemap
module ClangDep {
  header "ClangDep.h"
  export *
}

//--- Modules/ClangDep.h
typedef int my_int;


//--- Modules/Interopt.swiftmodule/arm64-apple-macos.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -target arm64-apple-macos16.4  
import Simple
import ClangDep

//--- Modules/Interopt.swiftmodule/arm64-apple-ios-macabi.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -target arm64-apple-ios22.0-macabi  
import Simple
import ClangDep

//--- input.swift
import Interopt

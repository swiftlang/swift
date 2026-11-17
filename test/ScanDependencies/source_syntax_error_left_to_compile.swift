// REQUIRES: swift_feature_ParserASTGen
// Verify that syntax errors and notes in scanned source files are suppressed
// during dependency scanning so the compile job reports them, while #if
// canImport diagnostics (including under -warnings-as-errors) are still
// reported by the scanner.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -scan-dependencies -module-name Test \
// RUN:   -module-cache-path %t/clang-module-cache -I %S/Inputs/Swift -I %S/Inputs/CHeaders \
// RUN:   %t/main.swift -o %t/deps.json 2>&1 | %FileCheck %s --check-prefix=DIAGS \
// RUN:   --implicit-check-not=error: --implicit-check-not=warning: --implicit-check-not=note:
// RUN: %FileCheck %s --input-file=%t/deps.json
// RUN: %target-swift-frontend -scan-dependencies -module-name Test \
// RUN:   -enable-experimental-feature ParserASTGen \
// RUN:   -module-cache-path %t/clang-module-cache -I %S/Inputs/Swift -I %S/Inputs/CHeaders \
// RUN:   %t/main.swift -o %t/deps_astgen.json 2>&1 | %FileCheck %s --check-prefix=DIAGS \
// RUN:   --implicit-check-not=error: --implicit-check-not=warning: --implicit-check-not=note:
// RUN: %FileCheck %s --input-file=%t/deps_astgen.json

// DIAGS: main.swift:3:20: warning: cannot find user version number for module 'A'; version number ignored
// DIAGS: `- warning: cannot find user version number for module 'A'; version number ignored
// DIAGS: main.swift:7:15: warning: cannot find module 'MissingModule' for canImport check; the directive will evaluate to false
// DIAGS: `- warning: cannot find module 'MissingModule' for canImport check; the directive will evaluate to false

// CHECK: "mainModuleName": "Test"
// CHECK: "swift": "A"

//--- main.swift
#if os(UNKNOWN_OS)
import NonExistent
#else if canImport(A, _version: 2)
import A
#endif

#if canImport(MissingModule, _version: 2)
import MissingModule
#endif

#error("leave to compile job")

class Unclosed {
  func f() {}

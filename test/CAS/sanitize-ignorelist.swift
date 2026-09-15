// Verify that the sanitizer ignorelist file that the Clang driver auto-injects
// for `-sanitize=address` is included in the CAS input tree for a cached
// `-compile-module-from-interface` job. Without this, replaying the cached
// compile fails to open the ignorelist file.

// REQUIRES: OS=macosx

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -sanitize=address \
// RUN:   -I %t/include \
// RUN:   %t/main.swift -o %t/deps.json -cache-compile-job -cas-path %t/cas

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json A > %t/A.cmd
// RUN: %FileCheck %s -check-prefix CMD -input-file=%t/A.cmd
// CMD: -fsanitize-ignorelist={{.*}}asan_ignorelist.txt

// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json A casFSRootID > %t/A.casid
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-include-tree-list @%t/A.casid | %FileCheck %s --check-prefix=A-FS
// A-FS: asan_ignorelist.txt

//--- main.swift
import A

//--- include/A.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name A -O -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -user-module-version 1.0
public func a() { }

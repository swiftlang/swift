// REQUIRES: OS=macosx, objc_interop
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Different version should not match.
// RUN: %target-swift-frontend -target %module-target-triple -scan-dependencies -module-name Test %t/main.swift -module-cache-path %t/clang-module-cache \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -O \
// RUN:   -o %t/deps-now.json -I %t/include
// RUN: %target-swift-frontend -target %module-target-future -scan-dependencies -module-name Test %t/main.swift -module-cache-path %t/clang-module-cache \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -O \
// RUN:   -o %t/deps-future.json -I %t/include
// RUN: %{python} %S/../CAS/Inputs/SwiftDepsExtractor.py %t/deps-now.json Library modulePath > %t/path-now
// RUN: %{python} %S/../CAS/Inputs/SwiftDepsExtractor.py %t/deps-future.json Library modulePath > %t/path-future
// RUN: not diff %t/path-now %t/path-future

/// Application extension should not match.
// RUN: %target-swift-frontend -scan-dependencies -module-name Test %t/main.swift -module-cache-path %t/clang-module-cache \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -O \
// RUN:   -o %t/deps.json -I %t/include
// RUN: %target-swift-frontend -scan-dependencies -module-name Test %t/main.swift -module-cache-path %t/clang-module-cache \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -O \
// RUN:   -o %t/deps-ae.json -I %t/include -application-extension
// RUN: %{python} %S/../CAS/Inputs/SwiftDepsExtractor.py %t/deps.json Library modulePath > %t/path
// RUN: %{python} %S/../CAS/Inputs/SwiftDepsExtractor.py %t/deps-ae.json Library modulePath > %t/path-ae
// RUN: not diff %t/path %t/path-ae

//--- main.swift
import Library

//--- include/Library.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name Library -O -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -user-module-version 1.0
import Swift
@_exported import A
@inlinable
public func test() {
    foo()
}

//--- include/a.h
void foo(void);

//--- include/module.modulemap
module A {
  header "a.h"
  export *
}

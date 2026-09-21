// REQUIRES: swift_feature_Embedded

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// A hosted target is used here because Darwin forces PIC regardless of
/// `-fno-pic`; on this triple the flag genuinely selects the static
/// relocation model, which is what triggers the check.
// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache \
// RUN:   -target x86_64-unknown-linux-gnu \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/test.swift -o %t/deps.json -cache-compile-job -cas-path %t/cas \
// RUN:   -enable-experimental-feature Embedded -wmo -parse-stdlib -Xcc -fno-pic \
// RUN:   -import-objc-header %t/Bridging.h

/// The scanner must propagate Embedded into the bridging header command line,
/// alongside the round-tripped non-PIC relocation model.
// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json Test bridgingHeader | %FileCheck %s --check-prefix=CMD

// CMD: "commandLine": [
// CMD-DAG: "-enable-experimental-feature"
// CMD-DAG: "static"

/// Emitting the PCH with that command line must succeed.
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json bridgingHeader > %t/header.cmd
// RUN: %target-swift-frontend-plain @%t/header.cmd %t/Bridging.h -disable-implicit-swift-modules -o %t/bridging.pch

//--- test.swift
public func test() {}

//--- Bridging.h
void b(void);

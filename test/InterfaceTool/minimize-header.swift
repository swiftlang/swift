// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %swift-interface-tool -action minimize %t/basic/input.swift > %t/basic/output.swift
// RUN: %diff -u %t/basic/expected.swift %t/basic/output.swift

// RUN: %swift-interface-tool -action minimize %t/interleaved/input.swift > %t/interleaved/output.swift
// RUN: %diff -u %t/interleaved/expected.swift %t/interleaved/output.swift

// RUN: %swift-interface-tool -action minimize %t/sentinel-not-first/input.swift > %t/sentinel-not-first/output.swift
// RUN: %diff -u %t/sentinel-not-first/expected.swift %t/sentinel-not-first/output.swift

// RUN: %swift-interface-tool -action minimize %t/doc-and-regular/input.swift > %t/doc-and-regular/output.swift
// RUN: %diff -u %t/doc-and-regular/expected.swift %t/doc-and-regular/output.swift

// RUN: %swift-interface-tool -action minimize %t/sentinel-after-code/input.swift > %t/sentinel-after-code/output.swift
// RUN: %diff -u %t/sentinel-after-code/expected.swift %t/sentinel-after-code/output.swift

//--- basic/input.swift
// swift-interface-format-version: 1.0
// swift-compiler-version: Apple Swift version 6.0
// swift-module-flags: -target arm64-apple-macos14.0 -enable-library-evolution -module-name MyModule
import BetaMod
import AlphaMod

/// Doc comment on public func.
public func publicFunc() {
  print("hello")
}

// Regular comment before private func.
private func privateFunc() {
  print("private")
}
//--- basic/expected.swift
// swift-interface-format-version: 1.0
// swift-compiler-version: Apple Swift version 6.0
// swift-module-flags: -target arm64-apple-macos14.0 -enable-library-evolution -module-name MyModule
import BetaMod
import AlphaMod
//--- interleaved/input.swift
// swift-interface-format-version: 1.0
// random non-swift comment
// swift-module-flags: -foo
// another non-swift comment
// swift-compiler-version: 6.0
import AlphaMod
//--- interleaved/expected.swift
// swift-interface-format-version: 1.0
// swift-module-flags: -foo
// swift-compiler-version: 6.0
import AlphaMod
//--- sentinel-not-first/input.swift
// preceding non-swift comment
// swift-interface-format-version: 1.0
// swift-module-flags: -foo
import AlphaMod
//--- sentinel-not-first/expected.swift
// swift-interface-format-version: 1.0
// swift-module-flags: -foo
import AlphaMod
//--- doc-and-regular/input.swift
/// Doc comment on import.
import AlphaMod
// Regular comment between imports.
import BetaMod

/// Doc on public func — should be dropped.
public func docAndRegularFoo() {}
//--- doc-and-regular/expected.swift
import AlphaMod
import BetaMod
//--- sentinel-after-code/input.swift
// Only the comments leading the file are a header. A `// swift-` comment that
// appears after code is an ordinary comment.
// swift-interface-format-version: 1.0
import AlphaMod
// swift-module-flags: -not-a-header
import BetaMod
//--- sentinel-after-code/expected.swift
// swift-interface-format-version: 1.0
import AlphaMod
import BetaMod

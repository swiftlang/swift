// A top-level `#if canImport(M, _version: v)` for a
// versionless module must emit `cannot_find_module_version` exactly once in
// ASTGen-only mode (`ParserASTGen`), matching the default C++-parser mode.
// In ASTGen-only mode the C++ parser's `EvaluateIfConditionRequest` never runs,
// so `parseSourceFileViaASTGen` performs the canonical evaluation instead.

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/textual)
// RUN: %empty-directory(%t/binary)
// RUN: echo "public func foo() {}" > %t/Foo.swift

// RUN: %target-swift-frontend -emit-module %t/Foo.swift \
// RUN:   -module-name NoUserModuleVersion -swift-version 5 \
// RUN:   -disable-implicit-concurrency-module-import \
// RUN:   -emit-module-interface-path %t/textual/NoUserModuleVersion.swiftinterface \
// RUN:   -enable-library-evolution \
// RUN:   -emit-module-path %t/binary/NoUserModuleVersion.swiftmodule

// RUN: %target-typecheck-verify-swift -disable-implicit-concurrency-module-import -I %t/textual
// RUN: %target-typecheck-verify-swift -disable-implicit-concurrency-module-import -I %t/binary
// RUN: %target-typecheck-verify-swift -disable-implicit-concurrency-module-import -I %t/textual -enable-experimental-feature ParserASTGen
// RUN: %target-typecheck-verify-swift -disable-implicit-concurrency-module-import -I %t/binary -enable-experimental-feature ParserASTGen

// REQUIRES: swift_feature_ParserASTGen

import NoUserModuleVersion

#if canImport(NoUserModuleVersion, _version: 113.331) // expected-warning {{cannot find user version number for module 'NoUserModuleVersion'; version number ignored}}
func active() {}
#endif

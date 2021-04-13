
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/MCP)

// This test makes sure that the module cache hash properly
// namespaces/distinguishes modules when recompiling from a .swiftinterface file
// depending on if:
//
// a. OSSA Modules are enabled or not.
// b. If OSSA Modules are enabled, additionally distinguish in between the various optimization levels.
//
// This will allow for users who want to work with ossa modules in their project
// to build with OSSA even if the original project was not compiled in OSSA
// mode.

// 1. Build a .swiftinterface for all the different optimization levels. These
// optimization levels are embedded in the swift interface file and control how
// we regenerate the module. As proven by the test
// different-mode-have-same-interface-file.swift in this directory, we know that
// the flag does not impact the swift interface file.

// RUN: %target-swift-frontend -typecheck -emit-module-interface-path %t/SwiftModuleDefault.swiftinterface -parse-stdlib %s -module-name SwiftModuleDefault -enable-library-evolution -swift-version 5
// RUN: %target-swift-frontend -typecheck -emit-module-interface-path %t/SwiftModuleOnone.swiftinterface -parse-stdlib %s -module-name SwiftModuleOnone -enable-library-evolution -swift-version 5 -Onone
// RUN: %target-swift-frontend -typecheck -emit-module-interface-path %t/SwiftModuleO.swiftinterface -parse-stdlib %s -module-name SwiftModuleO -enable-library-evolution -swift-version 5 -O
// RUN: %target-swift-frontend -typecheck -emit-module-interface-path %t/SwiftModuleOsize.swiftinterface -parse-stdlib %s -module-name SwiftModuleOsize -enable-library-evolution -swift-version 5 -Osize

// 2. Create some trivial programs that import the various libraries with the
// various opt levels and with/without expecting a rebuild remark from the
// compiler. The remark allows us another way to validate that we are rebuilding
// what we expect.

// RUN: echo "import SwiftModuleDefault // expected-remark {{rebuilding module 'SwiftModuleDefault'}}" > %t/test.default.remark.swift
// RUN: echo "import SwiftModuleOnone // expected-remark {{rebuilding module 'SwiftModuleOnone'}}" > %t/test.Onone.remark.swift
// RUN: echo "import SwiftModuleO // expected-remark {{rebuilding module 'SwiftModuleO'}}" > %t/test.O.remark.swift
// RUN: echo "import SwiftModuleOsize // expected-remark {{rebuilding module 'SwiftModuleOsize'}}" > %t/test.Osize.remark.swift

// RUN: echo 'import SwiftModuleDefault' > %t/test.default.swift
// RUN: echo 'import SwiftModuleOnone' > %t/test.Onone.swift
// RUN: echo 'import SwiftModuleO' > %t/test.O.swift
// RUN: echo 'import SwiftModuleOsize' > %t/test.Osize.swift

// 3. Then compile those helper test files and make sure we emit the appropriate
// swift modules.
//
// This means that we test the following:
//
// a. idempotence: That invoking twice with/without the emit-ossa-modules flag
// always doesn't result in a new swift interface being emitted.
//
// b. That that -emit-ossa-modules cause us to use a different swift module.
//
// c. When compiling without optimizations, the emit-ossa-modules does nothing
// and we always serialize SIL in OSSA form.
//
// d. That when compiling with optimizations, the emit-ossa-modules controls
// whether or not SIL is serialized in OSSA form.
//
// NOTE: What we compile the test file with does not matter. The swift interface
// file controls how the module is optimized.

///////////////////////////////
// No Optimization Specified //
///////////////////////////////

// RUN: %empty-directory(%t/MCP.default)
// RUN: %empty-directory(%t/MCP.default/old)
// RUN: ls %t/MCP.default | count 1
// RUN: %target-swift-frontend -typecheck -sdk '' -module-cache-path %t/MCP.default -parse-stdlib -I %t %t/test.default.remark.swift -Rmodule-interface-rebuild -verify
// RUN: ls %t/MCP.default | count 3
// RUN: %target-swift-frontend -typecheck -sdk '' -module-cache-path %t/MCP.default -parse-stdlib -I %t %t/test.default.swift -Rmodule-interface-rebuild -verify
// RUN: %target-sil-opt -module-name SwiftModuleDefault %t/MCP.default/*.swiftmodule | grep '@$s18SwiftModuleDefault3fooAA5KlassCyF' | grep '[[]ossa[]]'
// RUN: mv %t/MCP.default/*.swiftmodule %t/MCP.default/old
// RUN: ls %t/MCP.default | count 2
// RUN: %target-swift-frontend -typecheck -sdk '' -enable-ossa-modules -module-cache-path %t/MCP.default -parse-stdlib -I %t %t/test.default.remark.swift -Rmodule-interface-rebuild -verify
// RUN: ls %t/MCP.default | count 3
// RUN: %target-swift-frontend -typecheck -sdk '' -enable-ossa-modules -module-cache-path %t/MCP.default -parse-stdlib -I %t %t/test.default.swift -Rmodule-interface-rebuild -verify
// RUN: ls %t/MCP.default | count 3
//
// These should be the same.
// RUN: diff -u %t/MCP.default/*.swiftmodule %t/MCP.default/old/*.swiftmodule
//
// But their actual names should be different since the hash is in the file name.
// RUN: cd %t/MCP.default && ls *.swiftmodule > %t/MCP.default/firstFile
// RUN: cd %t/MCP.default/old && ls *.swiftmodule > %t/MCP.default/secondFile
// RUN: not diff -u %t/MCP.default/firstFile %t/MCP.default/secondFile

///////////
// Onone //
///////////

// RUN: %empty-directory(%t/MCP.Onone)
// RUN: %empty-directory(%t/MCP.Onone/old)
// RUN: ls %t/MCP.Onone | count 1
// RUN: %target-swift-frontend -typecheck -sdk '' -module-cache-path %t/MCP.Onone -parse-stdlib -I %t %t/test.Onone.remark.swift -Rmodule-interface-rebuild -verify
// RUN: ls %t/MCP.Onone | count 3
// RUN: %target-swift-frontend -typecheck -sdk '' -module-cache-path %t/MCP.Onone -parse-stdlib -I %t %t/test.Onone.swift -Rmodule-interface-rebuild -verify
// RUN: %target-sil-opt -module-name SwiftModuleOnone %t/MCP.Onone/*.swiftmodule | grep '@$s16SwiftModuleOnone3fooAA5KlassCyF' | grep '[[]ossa[]]'
// RUN: mv %t/MCP.Onone/*.swiftmodule %t/MCP.Onone/old
// RUN: ls %t/MCP.Onone | count 2
// RUN: %target-swift-frontend -typecheck -sdk '' -enable-ossa-modules -module-cache-path %t/MCP.Onone -parse-stdlib -I %t %t/test.Onone.remark.swift -Rmodule-interface-rebuild -verify
// RUN: ls %t/MCP.Onone | count 3
// RUN: %target-swift-frontend -typecheck -sdk '' -enable-ossa-modules -module-cache-path %t/MCP.Onone -parse-stdlib -I %t %t/test.Onone.swift -Rmodule-interface-rebuild -verify
// RUN: ls %t/MCP.Onone | count 3
//
// These should be the same.
// RUN: diff -u %t/MCP.Onone/*.swiftmodule %t/MCP.Onone/old/*.swiftmodule
//
// But their name should be different
// RUN: cd %t/MCP.Onone && ls *.swiftmodule > %t/MCP.Onone/firstFile
// RUN: cd %t/MCP.Onone/old && ls *.swiftmodule > %t/MCP.Onone/secondFile
// RUN: not diff -u %t/MCP.Onone/firstFile %t/MCP.Onone/secondFile

///////////
// Osize //
///////////

// RUN: %empty-directory(%t/MCP.Osize)
// RUN: %empty-directory(%t/MCP.Osize/old)
// RUN: ls %t/MCP.Osize | count 1
// RUN: %target-swift-frontend -typecheck -sdk '' -module-cache-path %t/MCP.Osize -parse-stdlib -I %t %t/test.Osize.remark.swift -Rmodule-interface-rebuild -verify
// RUN: ls %t/MCP.Osize | count 3
// RUN: %target-swift-frontend -typecheck -sdk '' -module-cache-path %t/MCP.Osize -parse-stdlib -I %t %t/test.Osize.swift -Rmodule-interface-rebuild -verify
// The grep at the end is an inverse so we are validating that the line doesn't have ossa.
// RUN: %target-sil-opt -module-name SwiftModuleOsize %t/MCP.Osize/*.swiftmodule | grep '@$s16SwiftModuleOsize3fooAA5KlassCyF' | grep -v '[[]ossa[]]'
// RUN: mv %t/MCP.Osize/*.swiftmodule %t/MCP.Osize/old
// RUN: ls %t/MCP.Osize | count 2
// RUN: %target-swift-frontend -typecheck -sdk '' -enable-ossa-modules -module-cache-path %t/MCP.Osize -parse-stdlib -I %t %t/test.Osize.remark.swift -Rmodule-interface-rebuild -verify
// RUN: ls %t/MCP.Osize | count 3
// RUN: %target-swift-frontend -typecheck -sdk '' -enable-ossa-modules -module-cache-path %t/MCP.Osize -parse-stdlib -I %t %t/test.Osize.swift -Rmodule-interface-rebuild -verify
// RUN: ls %t/MCP.Osize | count 3
// RUN: %target-sil-opt -module-name SwiftModuleOsize %t/MCP.Osize/*.swiftmodule | grep '@$s16SwiftModuleOsize3fooAA5KlassCyF' | grep '[[]ossa[]]'
//
// These should not be the same.
// RUN: not diff -u %t/MCP.Osize/*.swiftmodule %t/MCP.Osize/old/*.swiftmodule
//
// And their name should be different
// RUN: cd %t/MCP.Osize && ls *.swiftmodule > %t/MCP.Osize/firstFile
// RUN: cd %t/MCP.Osize/old && ls *.swiftmodule > %t/MCP.Osize/secondFile
// RUN: not diff -u %t/MCP.Osize/firstFile %t/MCP.Osize/secondFile

///////
// O //
///////

// RUN: %empty-directory(%t/MCP.O)
// RUN: %empty-directory(%t/MCP.O/old)
// RUN: ls %t/MCP.O | count 1
// RUN: %target-swift-frontend -typecheck -sdk '' -module-cache-path %t/MCP.O -parse-stdlib -I %t %t/test.O.remark.swift -Rmodule-interface-rebuild -verify
// RUN: ls %t/MCP.O | count 3
// RUN: %target-swift-frontend -typecheck -sdk '' -module-cache-path %t/MCP.O -parse-stdlib -I %t %t/test.O.swift -Rmodule-interface-rebuild -verify
// The grep at the end is an inverse so we are validating that the line doesn't have ossa.
// RUN: %target-sil-opt -module-name SwiftModuleO %t/MCP.O/*.swiftmodule | grep '@$s12SwiftModuleO3fooAA5KlassCyF' | grep -v '[[]ossa[]]'
// RUN: mv %t/MCP.O/*.swiftmodule %t/MCP.O/old
// RUN: ls %t/MCP.O | count 2
// RUN: %target-swift-frontend -typecheck -sdk '' -enable-ossa-modules -module-cache-path %t/MCP.O -parse-stdlib -I %t %t/test.O.remark.swift -Rmodule-interface-rebuild -verify
// RUN: ls %t/MCP.O | count 3
// RUN: %target-swift-frontend -typecheck -sdk '' -enable-ossa-modules -module-cache-path %t/MCP.O -parse-stdlib -I %t %t/test.O.swift -Rmodule-interface-rebuild -verify
// RUN: ls %t/MCP.O | count 3
// RUN: %target-sil-opt -module-name SwiftModuleO %t/MCP.O/*.swiftmodule | grep '@$s12SwiftModuleO3fooAA5KlassCyF' | grep '[[]ossa[]]'
//
// These should not be the same.
// RUN: not diff -u %t/MCP.O/*.swiftmodule %t/MCP.O/old/*.swiftmodule
//
// And their name should be different
// RUN: cd %t/MCP.O && ls *.swiftmodule > %t/MCP.O/firstFile
// RUN: cd %t/MCP.O/old && ls *.swiftmodule > %t/MCP.O/secondFile
// RUN: not diff -u %t/MCP.O/firstFile %t/MCP.O/secondFile

@_fixed_layout
public final class Klass {
  public init() {
  }
}

// This is the inlinable function that we are looking for.
@inlinable
public func foo() -> Klass {
  return Klass()
}

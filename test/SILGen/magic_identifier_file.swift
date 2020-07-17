// RUN: %empty-directory(%t)

// Build library in Swift 5 mode:
// RUN: %target-build-swift-dylib(%t/%target-library-name(MagicIdentifierFileSwift5)) -module-name MagicIdentifierFileSwift5 -emit-module-path %t/MagicIdentifierFileSwift5.swiftmodule -emit-module-interface-path %t/MagicIdentifierFileSwift5.swiftinterface -swift-version 5 -enable-library-evolution %S/Inputs/MagicIdentifierFileSwift.swift

// Build library in "Swift 6" mode:
// RUN: %target-build-swift-dylib(%t/%target-library-name(MagicIdentifierFileSwift6)) -module-name MagicIdentifierFileSwift6 -emit-module-path %t/MagicIdentifierFileSwift6.swiftmodule -emit-module-interface-path %t/MagicIdentifierFileSwift6.swiftinterface -swift-version 5 -enable-experimental-concise-pound-file -enable-library-evolution %S/Inputs/MagicIdentifierFileSwift.swift

// Test in Swift 5 mode:
// RUN: %target-swift-emit-silgen -I %t -module-name Foo %/s | %FileCheck --check-prefixes=BOTH,ABSOLUTE %s

// Test in "Swift 6" mode:
// RUN: %target-swift-emit-silgen -I %t -enable-experimental-concise-pound-file -module-name Foo %/s | %FileCheck --check-prefixes=BOTH,CONCISE %s

// Remove compiled modules so we test against interfaces:
// RUN: rm %t/MagicIdentifierFileSwift5.swiftmodule %t/MagicIdentifierFileSwift6.swiftmodule

// Test in Swift 5 mode:
// RUN: %target-swift-emit-silgen -I %t -module-name Foo %/s | %FileCheck --check-prefixes=BOTH,ABSOLUTE %s

// Test in "Swift 6" mode:
// RUN: %target-swift-emit-silgen -I %t -enable-experimental-concise-pound-file -module-name Foo %/s | %FileCheck --check-prefixes=BOTH,CONCISE %s

import MagicIdentifierFileSwift5
import MagicIdentifierFileSwift6

func directUse() {
// BOTH-LABEL: sil {{.*}} @$s3Foo9directUseyyF
  print(#file)
// ABSOLUTE: string_literal utf8 "SOURCE_DIR/test/SILGen/magic_identifier_file.swift"
// CONCISE: string_literal utf8 "Foo/magic_identifier_file.swift"
}

func indirectUse() {
// BOTH-LABEL: sil {{.*}} @$s3Foo11indirectUseyyF
  useIndirectly()
// ABSOLUTE: string_literal utf8 "SOURCE_DIR/test/SILGen/magic_identifier_file.swift"
// CONCISE: string_literal utf8 "Foo/magic_identifier_file.swift"
}

func swift5LibraryUse() {
// BOTH-LABEL: sil {{.*}} @$s3Foo16swift5LibraryUseyyF
  MagicIdentifierFileSwift5.useLibrary()
// BOTH: string_literal utf8 "SOURCE_DIR/test/SILGen/magic_identifier_file.swift"
}

func swift6LibraryUse() {
// BOTH-LABEL: sil {{.*}} @$s3Foo16swift6LibraryUseyyF
  MagicIdentifierFileSwift6.useLibrary()
// BOTH: string_literal utf8 "Foo/magic_identifier_file.swift"
}

func stdlibUse() {
// BOTH-LABEL: sil {{.*}} @$s3Foo9stdlibUseyyF
  fatalError()
// BOTH: string_literal utf8 "Foo/magic_identifier_file.swift"
}

func forceUnwrap(_ x: ()?) {
// BOTH-LABEL: sil {{.*}} @$s3Foo11forceUnwrapyyytSgF
  _ = x!
// BOTH: string_literal utf8 "Foo/magic_identifier_file.swift"
}

func forceTry(_ fn: () throws -> ()) {
// BOTH-LABEL: sil {{.*}} @$s3Foo8forceTryyyyyKXEF
  try! fn()
// BOTH: string_literal utf8 "Foo/magic_identifier_file.swift"
}

func useIndirectly(file: String = #file) {}

// CONCISE-LABEL: // Mappings from '#fileID' to '#filePath':
// CONCISE:       //   'Foo/magic_identifier_file.swift' => 'SOURCE_DIR/test/SILGen/magic_identifier_file.swift'


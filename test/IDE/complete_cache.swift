// RUN: rm -rf %t.ccp %t.ccp.bak %t.mcp

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=CLANG_UNQUAL_1 -completion-cache-path=%t.ccp -module-cache-path %t.mcp > %t.ccp1.compl.txt
// RUN: cp -r %t.ccp %t.ccp.bak
// RUN: %FileCheck %s -check-prefix=CLANG_CTYPES < %t.ccp1.compl.txt
// RUN: %FileCheck %s -check-prefix=CLANG_MACROS < %t.ccp1.compl.txt
// RUN: %FileCheck %s -check-prefix=CLANG_DARWIN < %t.ccp1.compl.txt
// RUN: %FileCheck %s -check-prefix=CLANG_DARWIN_NEG < %t.ccp1.compl.txt

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=CLANG_UNQUAL_1 -completion-cache-path=%t.ccp -module-cache-path %t.mcp > %t.ccp2.compl.txt
// RUN: %FileCheck %s -check-prefix=CLANG_CTYPES < %t.ccp2.compl.txt
// RUN: %FileCheck %s -check-prefix=CLANG_MACROS < %t.ccp2.compl.txt
// RUN: %FileCheck %s -check-prefix=CLANG_DARWIN < %t.ccp2.compl.txt
// RUN: %FileCheck %s -check-prefix=CLANG_DARWIN_NEG < %t.ccp2.compl.txt

// Check for modifications to cache
// RUN: diff -r -u %t.ccp %t.ccp.bak

// Check the individual cache items.
// RUN: %target-swift-ide-test -dump-completion-cache %t.ccp/macros-* | %FileCheck %s -check-prefix=CLANG_MACROS
// RUN: %target-swift-ide-test -dump-completion-cache %t.ccp/ctypes-* | %FileCheck %s -check-prefix=CLANG_CTYPES
// RUN: %target-swift-ide-test -dump-completion-cache %t.ccp/Darwin-* | %FileCheck %s -check-prefix=CLANG_DARWIN
// RUN: %target-swift-ide-test -dump-completion-cache %t.ccp/Darwin-* | %FileCheck %s -check-prefix=CLANG_DARWIN_NEG


// Qualified.
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=CLANG_QUAL_MACROS_1 -completion-cache-path=%t.ccp -module-cache-path %t.mcp > %t.macros.ccp1.compl.txt
// RUN: %FileCheck %s -check-prefix=CLANG_QUAL_MACROS_1 -check-prefix=CLANG_QUAL_MACROS_1-%target-runtime < %t.macros.ccp1.compl.txt
// RUN: diff -r -u %t.ccp %t.ccp.bak

// Check the individual cache item.
// RUN: %target-swift-ide-test -dump-completion-cache %t.ccp/macros-* | %FileCheck %s -check-prefix=CLANG_QUAL_MACROS_1


// Qualified with dot.
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=CLANG_QUAL_MACROS_2 -completion-cache-path=%t.ccp -module-cache-path %t.mcp > %t.macros2.ccp1.compl.txt
// RUN: %FileCheck %s -check-prefix=CLANG_QUAL_MACROS_2 -check-prefix=CLANG_QUAL_MACROS_2-%target-runtime < %t.macros2.ccp1.compl.txt

// Check the individual cache item.
// RUN: %target-swift-ide-test -dump-completion-cache %t.ccp/macros-dot-* | %FileCheck %s -check-prefix=CLANG_QUAL_MACROS_2

// Qualified private with dot.
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=CLANG_QUAL_STRING -completion-cache-path=%t.ccp -module-cache-path %t.mcp > %t.string.ccp1.compl.txt
// RUN: %FileCheck %s -check-prefix=CLANG_QUAL_STRING  < %t.string.ccp1.compl.txt


// Ensure the testable import showed up mangled correctly.
// RUN: ls %t.ccp/Darwin-testable*
// RUN: ls %t.ccp/AppKit-private*

// REQUIRES: executable_test
// REQUIRES: swift_tools_extra

import macros
import ctypes
@testable import Darwin
@_private(sourceFile: "AppKit.swift") import AppKit

// CLANG_CTYPES-DAG: Decl[Struct]/OtherModule[ctypes]/IsSystem:    FooStruct1[#FooStruct1#]{{; name=.+$}}
// CLANG_CTYPES-DAG: Decl[Struct]/OtherModule[ctypes]/IsSystem:    FooStruct2[#FooStruct2#]{{; name=.+$}}
// CLANG_CTYPES-DAG: Decl[Struct]/OtherModule[ctypes]/IsSystem: FooStruct3[#FooStruct3#]{{; name=.+$}}
// CLANG_CTYPES-DAG: Decl[Struct]/OtherModule[ctypes]/IsSystem: FooStruct4[#FooStruct4#]{{; name=.+$}}
// CLANG_CTYPES-DAG: Decl[Struct]/OtherModule[ctypes]/IsSystem:    FooStruct5[#FooStruct5#]{{; name=.+$}}
// CLANG_CTYPES-DAG: Decl[Struct]/OtherModule[ctypes]/IsSystem:    FooStruct6[#FooStruct6#]{{; name=.+$}}
// CLANG_CTYPES-DAG: Decl[TypeAlias]/OtherModule[ctypes]/IsSystem: FooStructTypedef1[#FooStruct2#]{{; name=.+$}}

// CLANG_MACROS-DAG: Decl[GlobalVar]/OtherModule[macros]/IsSystem: USES_MACRO_FROM_OTHER_MODULE_1[#Int32#]{{; name=.+$}}

// CLANG_DARWIN-DAG: Decl[TypeAlias]/OtherModule[Darwin.MacTypes]/IsSystem: FourCharCode[#UInt32#]{{; name=.+$}}
// CLANG_DARWIN_NEG-NOT: FixedPtr
// CLANG_DARWIN_NEG-NOT: UniCharCoun

func testClangModule() {
  #^CLANG_UNQUAL_1^#
}

func testCompleteModuleQualifiedMacros1() {
  macros.#^CLANG_QUAL_MACROS_1^#
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]/IsSystem: A_PI[#Double#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]/IsSystem: CF_STRING[#String#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]/IsSystem: EOF[#Int32#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]/IsSystem: GL_FALSE[#Int32#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]/IsSystem: GL_RGBA[#Int32#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]/IsSystem: GL_RGB[#Int32#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]/IsSystem: MINUS_THREE[#Int32#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]/IsSystem: M_PIf[#Float#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-objc-DAG: Decl[GlobalVar]/OtherModule[macros]/IsSystem: OBJC_STRING[#String#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]/IsSystem: USES_MACRO_FROM_OTHER_MODULE_1[#Int32#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]/IsSystem: UTF8_STRING[#String#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]/IsSystem: VERSION_STRING[#String#]{{; name=.+$}}
}

func testCompleteModuleQualifiedMacros2() {
  macros#^CLANG_QUAL_MACROS_2^#
// CLANG_QUAL_MACROS_2-DAG: Decl[GlobalVar]/OtherModule[macros]/IsSystem: .A_PI[#Double#]{{; name=.+$}}
// CLANG_QUAL_MACROS_2-DAG: Decl[GlobalVar]/OtherModule[macros]/IsSystem: .CF_STRING[#String#]{{; name=.+$}}
// CLANG_QUAL_MACROS_2-DAG: Decl[GlobalVar]/OtherModule[macros]/IsSystem: .EOF[#Int32#]{{; name=.+$}}
// CLANG_QUAL_MACROS_2-DAG: Decl[GlobalVar]/OtherModule[macros]/IsSystem: .GL_FALSE[#Int32#]{{; name=.+$}}
// CLANG_QUAL_MACROS_2-DAG: Decl[GlobalVar]/OtherModule[macros]/IsSystem: .GL_RGBA[#Int32#]{{; name=.+$}}
// CLANG_QUAL_MACROS_2-DAG: Decl[GlobalVar]/OtherModule[macros]/IsSystem: .GL_RGB[#Int32#]{{; name=.+$}}
// CLANG_QUAL_MACROS_2-DAG: Decl[GlobalVar]/OtherModule[macros]/IsSystem: .MINUS_THREE[#Int32#]{{; name=.+$}}
// CLANG_QUAL_MACROS_2-DAG: Decl[GlobalVar]/OtherModule[macros]/IsSystem: .M_PIf[#Float#]{{; name=.+$}}
// CLANG_QUAL_MACROS_2-objc-DAG: Decl[GlobalVar]/OtherModule[macros]/IsSystem: .OBJC_STRING[#String#]{{; name=.+$}}
// CLANG_QUAL_MACROS_2-DAG: Decl[GlobalVar]/OtherModule[macros]/IsSystem: .USES_MACRO_FROM_OTHER_MODULE_1[#Int32#]{{; name=.+$}}
// CLANG_QUAL_MACROS_2-DAG: Decl[GlobalVar]/OtherModule[macros]/IsSystem: .UTF8_STRING[#String#]{{; name=.+$}}
// CLANG_QUAL_MACROS_2-DAG: Decl[GlobalVar]/OtherModule[macros]/IsSystem: .VERSION_STRING[#String#]{{; name=.+$}}
}

func testPrivate() {
  String.#^CLANG_QUAL_STRING^#
// CLANG_QUAL_STRING: name=someMethod()
}

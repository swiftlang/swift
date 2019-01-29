// RUN: rm -rf %t.ccp %t.ccp.bak

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=CLANG_UNQUAL_1 -completion-cache-path=%t.ccp > %t.ccp1.compl.txt
// RUN: cp -r %t.ccp %t.ccp.bak
// RUN: %FileCheck %s -check-prefix=CLANG_CTYPES < %t.ccp1.compl.txt
// RUN: %FileCheck %s -check-prefix=CLANG_MACROS < %t.ccp1.compl.txt
// RUN: %FileCheck %s -check-prefix=CLANG_DARWIN < %t.ccp1.compl.txt
// RUN: %FileCheck %s -check-prefix=CLANG_DARWIN_NEG < %t.ccp1.compl.txt

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=CLANG_UNQUAL_1 -completion-cache-path=%t.ccp > %t.ccp2.compl.txt
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
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=CLANG_QUAL_MACROS_1 -completion-cache-path=%t.ccp > %t.macros.ccp1.compl.txt
// RUN: %FileCheck %s -check-prefix=CLANG_QUAL_MACROS_1 -check-prefix=CLANG_QUAL_MACROS_1-%target-runtime < %t.macros.ccp1.compl.txt
// RUN: diff -r -u %t.ccp %t.ccp.bak

// Check the individual cache item.
// RUN: %target-swift-ide-test -dump-completion-cache %t.ccp/macros-* | %FileCheck %s -check-prefix=CLANG_QUAL_MACROS_1


// Qualified with dot.
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=CLANG_QUAL_MACROS_2 -completion-cache-path=%t.ccp > %t.macros2.ccp1.compl.txt
// RUN: %FileCheck %s -check-prefix=CLANG_QUAL_MACROS_2 -check-prefix=CLANG_QUAL_MACROS_2-%target-runtime < %t.macros2.ccp1.compl.txt

// Check the individual cache item.
// RUN: %target-swift-ide-test -dump-completion-cache %t.ccp/macros-dot-* | %FileCheck %s -check-prefix=CLANG_QUAL_MACROS_2

// Qualified private with dot.
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=CLANG_QUAL_STRING -completion-cache-path=%t.ccp > %t.string.ccp1.compl.txt
// RUN: %FileCheck %s -check-prefix=CLANG_QUAL_STRING  < %t.string.ccp1.compl.txt


// Ensure the testable import showed up mangled correctly.
// RUN: ls %t.ccp/Darwin-testable*
// RUN: ls %t.ccp/AppKit-private*
// REQUIRES: executable_test

import macros
import ctypes
@testable import Darwin
@_private(sourceFile: "AppKit.swift") import AppKit

// CLANG_CTYPES: Begin completions
// CLANG_CTYPES-DAG: Decl[Struct]/OtherModule[ctypes]/keyword[Foo1, Struct1]:    FooStruct1[#FooStruct1#]{{; name=.+$}}
// CLANG_CTYPES-DAG: Decl[Struct]/OtherModule[ctypes]/keyword[Foo2]:    FooStruct2[#FooStruct2#]{{; name=.+$}}
// CLANG_CTYPES-DAG: Decl[Struct]/OtherModule[ctypes]/recommended[Foo2, Foo1]: FooStruct3[#FooStruct3#]{{; name=.+$}}
// CLANG_CTYPES-DAG: Decl[Struct]/OtherModule[ctypes]/recommendedover[Foo3, Foo2]: FooStruct4[#FooStruct4#]{{; name=.+$}}
// CLANG_CTYPES-DAG: Decl[Struct]/OtherModule[ctypes]:    FooStruct5[#FooStruct5#]{{; name=.+$}}
// CLANG_CTYPES-DAG: Decl[Struct]/OtherModule[ctypes]/recommendedover[ro1, ro2, ro3, ro4]/recommended[r1, r2, r3]/keyword[k1, k2, k3, k4]:    FooStruct6[#FooStruct6#]{{; name=.+$}}
// CLANG_CTYPES-DAG: Decl[TypeAlias]/OtherModule[ctypes]: FooStructTypedef1[#FooStruct2#]{{; name=.+$}}
// CLANG_CTYPES: End completions

// CLANG_MACROS: Begin completions
// CLANG_MACROS-DAG: Decl[GlobalVar]/OtherModule[macros]: USES_MACRO_FROM_OTHER_MODULE_1[#Int32#]{{; name=.+$}}
// CLANG_MACROS: End completions

// CLANG_DARWIN: Begin completions
// CLANG_DARWIN-DAG: Decl[TypeAlias]/OtherModule[Darwin.MacTypes]: FourCharCode[#UInt32#]{{; name=.+$}}
// CLANG_DARWIN_NEG-NOT: FixedPtr
// CLANG_DARWIN_NEG-NOT: UniCharCoun
// CLANG_DARWIN: End completions

func testClangModule() {
  #^CLANG_UNQUAL_1^#
}

func testCompleteModuleQualifiedMacros1() {
  macros.#^CLANG_QUAL_MACROS_1^#
// CLANG_QUAL_MACROS_1: Begin completions
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]: A_PI[#Double#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]: CF_STRING[#String#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]: EOF[#Int32#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]: GL_FALSE[#Int32#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]: GL_RGBA[#Int32#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]: GL_RGB[#Int32#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]: MINUS_THREE[#Int32#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]: M_PIf[#Float#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-objc-DAG: Decl[GlobalVar]/OtherModule[macros]: OBJC_STRING[#String#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]: USES_MACRO_FROM_OTHER_MODULE_1[#Int32#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]: UTF8_STRING[#String#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]: VERSION_STRING[#String#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1: End completions
}

func testCompleteModuleQualifiedMacros2() {
  macros#^CLANG_QUAL_MACROS_2^#
// CLANG_QUAL_MACROS_2: Begin completions
// CLANG_QUAL_MACROS_2-DAG: Decl[GlobalVar]/OtherModule[macros]: .A_PI[#Double#]{{; name=.+$}}
// CLANG_QUAL_MACROS_2-DAG: Decl[GlobalVar]/OtherModule[macros]: .CF_STRING[#String#]{{; name=.+$}}
// CLANG_QUAL_MACROS_2-DAG: Decl[GlobalVar]/OtherModule[macros]: .EOF[#Int32#]{{; name=.+$}}
// CLANG_QUAL_MACROS_2-DAG: Decl[GlobalVar]/OtherModule[macros]: .GL_FALSE[#Int32#]{{; name=.+$}}
// CLANG_QUAL_MACROS_2-DAG: Decl[GlobalVar]/OtherModule[macros]: .GL_RGBA[#Int32#]{{; name=.+$}}
// CLANG_QUAL_MACROS_2-DAG: Decl[GlobalVar]/OtherModule[macros]: .GL_RGB[#Int32#]{{; name=.+$}}
// CLANG_QUAL_MACROS_2-DAG: Decl[GlobalVar]/OtherModule[macros]: .MINUS_THREE[#Int32#]{{; name=.+$}}
// CLANG_QUAL_MACROS_2-DAG: Decl[GlobalVar]/OtherModule[macros]: .M_PIf[#Float#]{{; name=.+$}}
// CLANG_QUAL_MACROS_2-objc-DAG: Decl[GlobalVar]/OtherModule[macros]: .OBJC_STRING[#String#]{{; name=.+$}}
// CLANG_QUAL_MACROS_2-DAG: Decl[GlobalVar]/OtherModule[macros]: .USES_MACRO_FROM_OTHER_MODULE_1[#Int32#]{{; name=.+$}}
// CLANG_QUAL_MACROS_2-DAG: Decl[GlobalVar]/OtherModule[macros]: .UTF8_STRING[#String#]{{; name=.+$}}
// CLANG_QUAL_MACROS_2-DAG: Decl[GlobalVar]/OtherModule[macros]: .VERSION_STRING[#String#]{{; name=.+$}}
// CLANG_QUAL_MACROS_2: End completions
}

func testPrivate() {
  String.#^CLANG_QUAL_STRING^#
// CLANG_QUAL_STRING: Begin completions
// CLANG_QUAL_STRING: name=someMethod()
// CLANG_QUAL_STRING: End completions
}

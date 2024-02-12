// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=CLANG_UNQUAL_1 > %t.compl.txt
// RUN: %FileCheck %s -check-prefix=CLANG_CTYPES < %t.compl.txt
// RUN: %FileCheck %s -check-prefix=CLANG_MACROS < %t.compl.txt
// RUN: %FileCheck %s -check-prefix=CLANG_DARWIN < %t.compl.txt
// RUN: %FileCheck %s -check-prefix=CLANG_DARWIN_NEG < %t.compl.txt

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=CLANG_MEMBER1 > %t.compl.txt
// RUN: %FileCheck %s -check-prefix=CLANG_MEMBERS1 < %t.compl.txt

import macros
import ctypes
import Darwin

// CLANG_CTYPES-DAG: Decl[Struct]/OtherModule[ctypes]/IsSystem:    FooStruct1[#FooStruct1#]{{; name=.+$}}
// CLANG_CTYPES-DAG: Decl[Struct]/OtherModule[ctypes]/IsSystem:    FooStruct2[#FooStruct2#]{{; name=.+$}}
// CLANG_CTYPES-DAG: Decl[Struct]/OtherModule[ctypes]/IsSystem:    FooStruct3[#FooStruct3#]{{; name=.+$}}
// CLANG_CTYPES-DAG: Decl[Struct]/OtherModule[ctypes]/IsSystem:    FooStruct4[#FooStruct4#]{{; name=.+$}}
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
// CLANG_QUAL_MACROS_1: Begin completions, 16 items
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]/IsSystem: A_PI[#CDouble#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]/IsSystem: CF_STRING[#CString#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]/IsSystem: EOF[#Int32#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]/IsSystem: GL_FALSE[#Int32#]{{$}`}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]/IsSystem: GL_RGBA[#CInt#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]/IsSystem: GL_RGB[#CInt#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]/IsSystem: INT64_MAX[#CLongLong#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]/IsSystem: MACRO_FROM_IMPL[#CInt#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]/IsSystem: MINUS_THREE[#CInt#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]/IsSystem: M_PIf[#CFloat#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]/IsSystem: OBJC_STRING[#String#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]/IsSystem: UINT32_MAX[#CUnsignedInt#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]/IsSystem: USES_MACRO_FROM_OTHER_MODULE_1[#CInt#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]/IsSystem: UTF8_STRING[#CString#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]/IsSystem: VERSION_STRING[#CString#]{{; name=.+$}}
}

func testClangMember1() {
	var FS = FooStruct1()
	FS.#^CLANG_MEMBER1^#
// CLANG_MEMBERS1: Begin completions, 3 items
// CLANG_MEMBERS1-DAG: Decl[InstanceVar]/CurrNominal/IsSystem: x[#Int32#]{{; name=.+$}}
// CLANG_MEMBERS1-DAG: Decl[InstanceVar]/CurrNominal/IsSystem: y[#Double#]{{; name=.+$}}
// CLANG_MEMBERS1-DAG: Keyword[self]/CurrNominal: self[#FooStruct1#]; name=self
}

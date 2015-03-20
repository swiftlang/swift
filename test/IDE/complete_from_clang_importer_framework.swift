// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=CLANG_UNQUAL_1 > %t.compl.txt
// RUN: FileCheck %s -check-prefix=CLANG_CTYPES < %t.compl.txt
// RUN: FileCheck %s -check-prefix=CLANG_MACROS < %t.compl.txt
// RUN: FileCheck %s -check-prefix=CLANG_DARWIN < %t.compl.txt
// RUN: FileCheck %s -check-prefix=CLANG_DARWIN_NEG < %t.compl.txt
// XFAIL: *

import macros
import ctypes
import Darwin

// CLANG_CTYPES: Begin completions
// CLANG_CTYPES-DAG: Decl[Struct]/OtherModule[ctypes]:    FooStruct1[#FooStruct1#]{{; name=.+$}}
// CLANG_CTYPES-DAG: Decl[Struct]/OtherModule[ctypes]:    FooStruct2[#FooStruct2#]{{; name=.+$}}
// CLANG_CTYPES-DAG: Decl[Struct]/OtherModule[ctypes]:    FooStruct3[#FooStruct3#]{{; name=.+$}}
// CLANG_CTYPES-DAG: Decl[Struct]/OtherModule[ctypes]:    FooStruct4[#FooStruct4#]{{; name=.+$}}
// CLANG_CTYPES-DAG: Decl[Struct]/OtherModule[ctypes]:    FooStruct5[#FooStruct5#]{{; name=.+$}}
// CLANG_CTYPES-DAG: Decl[Struct]/OtherModule[ctypes]:    FooStruct6[#FooStruct6#]{{; name=.+$}}
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
// CLANG_QUAL_MACROS_1: Begin completions, 16 items
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]: A_PI[#CDouble#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]: CF_STRING[#CString#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]: EOF[#Int32#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]: GL_FALSE[#Int32#]{{$}`}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]: GL_RGBA[#CInt#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]: GL_RGB[#CInt#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]: INT64_MAX[#CLongLong#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]: MACRO_FROM_IMPL[#CInt#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]: MINUS_THREE[#CInt#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]: M_PIf[#CFloat#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]: OBJC_STRING[#String#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]: UINT32_MAX[#CUnsignedInt#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]: USES_MACRO_FROM_OTHER_MODULE_1[#CInt#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]: UTF8_STRING[#CString#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule[macros]: VERSION_STRING[#CString#]{{; name=.+$}}
// CLANG_QUAL_MACROS_1: End completions
}

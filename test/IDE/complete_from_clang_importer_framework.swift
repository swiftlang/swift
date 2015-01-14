// RUN: %swift-ide-test -code-completion -source-filename %s %clang-importer-sdk -target x86_64-apple-macosx10.9 -code-completion-token=CLANG_UNQUAL_1 > %t.compl.txt
// RUN: FileCheck %s -check-prefix=CLANG_CTYPES < %t.compl.txt
// RUN: FileCheck %s -check-prefix=CLANG_MACROS < %t.compl.txt
// RUN: FileCheck %s -check-prefix=CLANG_DARWIN < %t.compl.txt
// RUN: FileCheck %s -check-prefix=CLANG_DARWIN_NEG < %t.compl.txt

import macros
import ctypes
import Darwin

// CLANG_CTYPES: Begin completions
// CLANG_CTYPES-DAG: Decl[Struct]/OtherModule:    FooStruct1[#FooStruct1#]{{$}}
// CLANG_CTYPES-DAG: Decl[Struct]/OtherModule:    FooStruct2[#FooStruct2#]{{$}}
// CLANG_CTYPES-DAG: Decl[Struct]/OtherModule:    FooStruct3[#FooStruct3#]{{$}}
// CLANG_CTYPES-DAG: Decl[Struct]/OtherModule:    FooStruct4[#FooStruct4#]{{$}}
// CLANG_CTYPES-DAG: Decl[Struct]/OtherModule:    FooStruct5[#FooStruct5#]{{$}}
// CLANG_CTYPES-DAG: Decl[Struct]/OtherModule:    FooStruct6[#FooStruct6#]{{$}}
// CLANG_CTYPES-DAG: Decl[TypeAlias]/OtherModule: FooStructTypedef1[#FooStruct2#]{{$}}
// CLANG_CTYPES: End completions

// CLANG_MACROS: Begin completions
// CLANG_MACROS-DAG: Decl[GlobalVar]/OtherModule: USES_MACRO_FROM_OTHER_MODULE_1[#Int32#]{{$}}
// CLANG_MACROS: End completions

// CLANG_DARWIN: Begin completions
// CLANG_DARWIN-DAG: Decl[TypeAlias]/OtherModule: FourCharCode[#UInt32#]{{$}}
// CLANG_DARWIN_NEG-NOT: FixedPtr
// CLANG_DARWIN_NEG-NOT: UniCharCoun
// CLANG_DARWIN: End completions

func testClangModule() {
  #^CLANG_UNQUAL_1^#
}

func testCompleteModuleQualifiedMacros1() {
  macros.#^CLANG_QUAL_MACROS_1^#
// CLANG_QUAL_MACROS_1: Begin completions, 16 items
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule: A_PI[#CDouble#]{{$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule: CF_STRING[#CString#]{{$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule: EOF[#Int32#]{{$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule: GL_FALSE[#Int32#]{{$}`}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule: GL_RGBA[#CInt#]{{$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule: GL_RGB[#CInt#]{{$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule: INT64_MAX[#CLongLong#]{{$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule: MACRO_FROM_IMPL[#CInt#]{{$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule: MINUS_THREE[#CInt#]{{$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule: M_PIf[#CFloat#]{{$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule: OBJC_STRING[#String#]{{$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule: UINT32_MAX[#CUnsignedInt#]{{$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule: USES_MACRO_FROM_OTHER_MODULE_1[#CInt#]{{$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule: UTF8_STRING[#CString#]{{$}}
// CLANG_QUAL_MACROS_1-DAG: Decl[GlobalVar]/OtherModule: VERSION_STRING[#CString#]{{$}}
// CLANG_QUAL_MACROS_1: End completions
}



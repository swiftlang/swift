// RUN: %swift-ide-test -code-completion -source-filename %s -I %S/Inputs/custom-modules -code-completion-token=CLANG_UNQUAL_1 | FileCheck %s -check-prefix=CLANG_UNQUAL_1

import FooClangModule

struct SwiftFooStruct {
  var instanceVar : Int
}

// Test that we don't include Clang completions in unexpected places.
func testSwiftCompletions(foo : SwiftFooStruct) {
  foo.#^SWIFT_COMPLETIONS^#
// SWIFT_COMPLETIONS: Begin completions
// SWIFT_COMPLETIONS-NEXT: SwiftDecl: instanceVar[#Int#]
// SWIFT_COMPLETIONS-NEXT: Keyword: metatype[#[byref(implicit)] SwiftFooStruct.metatype#]
// SWIFT_COMPLETIONS-NEXT: End completions
}

func testClangModule() {
  #^CLANG_UNQUAL_1^#
// CLANG_UNQUAL_1: Begin completions
// CLANG_UNQUAL_1-DAG: SwiftDecl: FooEnum1[#FooEnum1.metatype#]{{$}}
// CLANG_UNQUAL_1-DAG: SwiftDecl: FooEnum1X[#FooEnum1#]{{$}}
// CLANG_UNQUAL_1-DAG: SwiftDecl: FooEnum2[#FooEnum2.metatype#]{{$}}
// CLANG_UNQUAL_1-DAG: SwiftDecl: FooEnum2Y[#FooEnum2#]{{$}}
// CLANG_UNQUAL_1-DAG: SwiftDecl: FooEnum2X[#FooEnum2#]{{$}}
// CLANG_UNQUAL_1-DAG: SwiftDecl: FooEnum3[#FooEnum3.metatype#]{{$}}
// CLANG_UNQUAL_1-DAG: SwiftDecl: FooEnum3Y[#FooEnum3#]{{$}}
// CLANG_UNQUAL_1-DAG: SwiftDecl: FooEnum3X[#FooEnum3#]{{$}}
// CLANG_UNQUAL_1-DAG: SwiftDecl: FooStruct1[#FooStruct1.metatype#]{{$}}
// CLANG_UNQUAL_1-DAG: SwiftDecl: FooStruct2[#FooStruct2.metatype#]{{$}}
// CLANG_UNQUAL_1-DAG: SwiftDecl: FooStructTypedef1[#FooStruct2.metatype#]{{$}}
// CLANG_UNQUAL_1-DAG: SwiftDecl: FooStructTypedef2[#FooStructTypedef2.metatype#]{{$}}
// CLANG_UNQUAL_1-DAG: SwiftDecl: FooTypedef1[#CInt.metatype#]{{$}}
// CLANG_UNQUAL_1-DAG: SwiftDecl: fooIntVar[#CInt#]{{$}}
// CLANG_UNQUAL_1-DAG: SwiftDecl: fooFunc1({#a: CInt#}, {#b: CFloat#}, {#c: CDouble#}, {#d: UnsafePointer<CInt>#})[#CInt#]{{$}}
// CLANG_UNQUAL_1: End completions
}


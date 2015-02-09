// RUN: %target-swift-ide-test -code-completion -source-filename %s -F %S/Inputs/mock-sdk -code-completion-token=SWIFT_COMPLETIONS | FileCheck %s -check-prefix=SWIFT_COMPLETIONS

// RUN: %target-swift-ide-test -code-completion -source-filename %s -F %S/Inputs/mock-sdk -code-completion-token=FW_UNQUAL_1 > %t.compl.txt
// RUN: FileCheck %s -check-prefix=CLANG_FOO < %t.compl.txt
// RUN: FileCheck %s -check-prefix=CLANG_FOO_SUB < %t.compl.txt
// RUN: FileCheck %s -check-prefix=CLANG_FOO_HELPER < %t.compl.txt
// RUN: FileCheck %s -check-prefix=CLANG_FOO_HELPER_SUB < %t.compl.txt
// RUN: FileCheck %s -check-prefix=CLANG_BAR < %t.compl.txt
// RUN: FileCheck %s -check-prefix=CLANG_BOTH_FOO_BAR < %t.compl.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -F %S/Inputs/mock-sdk -code-completion-token=CLANG_QUAL_FOO_1 > %t.compl.txt
// RUN: FileCheck %s -check-prefix=CLANG_FOO < %t.compl.txt
// RUN: FileCheck %s -check-prefix=CLANG_FOO_SUB < %t.compl.txt
// RUN: FileCheck %s -check-prefix=CLANG_QUAL_FOO_NEGATIVE < %t.compl.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -F %S/Inputs/mock-sdk -code-completion-token=CLANG_QUAL_BAR_1 > %t.compl.txt
// RUN: FileCheck %s -check-prefix=CLANG_QUAL_BAR_1 < %t.compl.txt
// RUN: FileCheck %s -check-prefix=CLANG_BAR < %t.compl.txt
// RUN: FileCheck %s -check-prefix=CLANG_QUAL_BAR_NEGATIVE < %t.compl.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -F %S/Inputs/mock-sdk -code-completion-token=CLANG_QUAL_FOO_2 | FileCheck %s -check-prefix=CLANG_QUAL_FOO_2

// RUN: %target-swift-ide-test -code-completion -source-filename %s -F %S/Inputs/mock-sdk -code-completion-token=FUNCTION_CALL_1 | FileCheck %s -check-prefix=FUNCTION_CALL_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -F %S/Inputs/mock-sdk -code-completion-token=FUNCTION_CALL_2 | FileCheck %s -check-prefix=FUNCTION_CALL_2

// RUN: %target-swift-ide-test -code-completion -source-filename %s -F %S/Inputs/mock-sdk -code-completion-token=CLANG_STRUCT_MEMBERS_1 | FileCheck %s -check-prefix=CLANG_STRUCT_MEMBERS_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -F %S/Inputs/mock-sdk -code-completion-token=CLANG_CLASS_MEMBERS_1 | FileCheck %s -check-prefix=CLANG_CLASS_MEMBERS_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -F %S/Inputs/mock-sdk -code-completion-token=CLANG_CLASS_MEMBERS_2 | FileCheck %s -check-prefix=CLANG_CLASS_MEMBERS_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -F %S/Inputs/mock-sdk -code-completion-token=CLANG_INSTANCE_MEMBERS_1 | FileCheck %s -check-prefix=CLANG_INSTANCE_MEMBERS_1

// XFAIL: linux

import Foo
// Don't import FooHelper directly in this test!
// import FooHelper
// Framework 'Foo' re-exports the 'FooHelper' framework.  Make sure that we get
// completion results for both frameworks.

import Bar

struct SwiftStruct {
  var instanceVar : Int
}

// Test that we don't include Clang completions in unexpected places.
func testSwiftCompletions(foo: SwiftStruct) {
  foo.#^SWIFT_COMPLETIONS^#
// SWIFT_COMPLETIONS: Begin completions
// SWIFT_COMPLETIONS-NEXT: Decl[InstanceVar]/CurrNominal: instanceVar[#Int#]{{; name=.+$}}
// SWIFT_COMPLETIONS-NEXT: End completions
}

// CLANG_FOO: Begin completions
// CLANG_FOO-DAG: Decl[Struct]/OtherModule:       FooEnum1[#FooEnum1#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[GlobalVar]/OtherModule:    FooEnum1X[#FooEnum1#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[Struct]/OtherModule:       FooEnum2[#FooEnum2#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[GlobalVar]/OtherModule:    FooEnum2X[#FooEnum2#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[GlobalVar]/OtherModule:    FooEnum2Y[#FooEnum2#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[Struct]/OtherModule:       FooEnum3[#FooEnum3#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[GlobalVar]/OtherModule:    FooEnum3X[#FooEnum3#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[GlobalVar]/OtherModule:    FooEnum3Y[#FooEnum3#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[Enum]/OtherModule:         FooComparisonResult[#FooComparisonResult#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[Struct]/OtherModule:       FooRuncingOptions[#FooRuncingOptions#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[Struct]/OtherModule:       FooStruct1[#FooStruct1#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[Struct]/OtherModule:       FooStruct2[#FooStruct2#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[TypeAlias]/OtherModule:    FooStructTypedef1[#FooStruct2#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[Struct]/OtherModule:       FooStructTypedef2[#FooStructTypedef2#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[TypeAlias]/OtherModule:    FooTypedef1[#Int32#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[GlobalVar]/OtherModule:    fooIntVar[#Int32#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[FreeFunction]/OtherModule: fooFunc1({#(a): Int32#})[#Int32#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[FreeFunction]/OtherModule: fooFunc1AnonymousParam({#Int32#})[#Int32#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[FreeFunction]/OtherModule: fooFunc3({#(a): Int32#}, {#(b): Float#}, {#(c): Double#}, {#(d): UnsafeMutablePointer<Int32>#})[#Int32#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[FreeFunction]/OtherModule: fooFuncWithBlock({#(blk): ((Float) -> Int32)!##(Float) -> Int32#})[#Void#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[FreeFunction]/OtherModule: fooFuncWithComment1()[#Void#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[FreeFunction]/OtherModule: fooFuncWithComment2()[#Void#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[FreeFunction]/OtherModule: fooFuncWithComment3()[#Void#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[FreeFunction]/OtherModule: fooFuncWithComment4()[#Void#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[FreeFunction]/OtherModule: fooFuncWithComment5()[#Void#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[Protocol]/OtherModule:     FooProtocolBase[#FooProtocolBase#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[Protocol]/OtherModule:     FooProtocolDerived[#FooProtocolDerived#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[Class]/OtherModule:        FooClassBase[#FooClassBase#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[Class]/OtherModule:        FooClassDerived[#FooClassDerived#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[GlobalVar]/OtherModule:    FOO_MACRO_1[#Int32#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[GlobalVar]/OtherModule:    FOO_MACRO_2[#Int32#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[GlobalVar]/OtherModule:    FOO_MACRO_3[#Int32#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[GlobalVar]/OtherModule:    FOO_MACRO_4[#UInt32#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[GlobalVar]/OtherModule:    FOO_MACRO_5[#UInt64#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[GlobalVar]/OtherModule:    FOO_MACRO_REDEF_1[#Int32#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[GlobalVar]/OtherModule:    FOO_MACRO_REDEF_2[#Int32#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[FreeFunction]/OtherModule: theLastDeclInFoo()[#Void#]{{; name=.+$}}
// CLANG_FOO: End completions

// CLANG_FOO_SUB: Begin completions
// CLANG_FOO_SUB-DAG: Decl[FreeFunction]/OtherModule: fooSubFunc1({#(a): Int32#})[#Int32#]{{; name=.+$}}
// CLANG_FOO_SUB-DAG: Decl[Struct]/OtherModule:       FooSubEnum1[#FooSubEnum1#]{{; name=.+$}}
// CLANG_FOO_SUB-DAG: Decl[GlobalVar]/OtherModule:    FooSubEnum1X[#FooSubEnum1#]{{; name=.+$}}
// CLANG_FOO_SUB-DAG: Decl[GlobalVar]/OtherModule:    FooSubEnum1Y[#FooSubEnum1#]{{; name=.+$}}
// CLANG_FOO_SUB-DAG: Decl[GlobalVar]/OtherModule:    FooSubUnnamedEnumeratorA1[#Int#]{{; name=.+$}}
// CLANG_FOO_SUB: End completions

// CLANG_FOO_HELPER: Begin completions
// CLANG_FOO_HELPER-DAG: Decl[FreeFunction]/OtherModule: fooHelperFunc1({#(a): Int32#})[#Int32#]{{; name=.+$}}
// CLANG_FOO_HELPER-DAG: Decl[GlobalVar]/OtherModule:    FooHelperUnnamedEnumeratorA1[#Int#]{{; name=.+$}}
// CLANG_FOO_HELPER-DAG: Decl[GlobalVar]/OtherModule:    FooHelperUnnamedEnumeratorA2[#Int#]{{; name=.+$}}
// CLANG_FOO_HELPER: End completions

// CLANG_FOO_HELPER_SUB: Begin completions
// CLANG_FOO_HELPER_SUB-DAG: Decl[FreeFunction]/OtherModule: fooHelperSubFunc1({#(a): Int32#})[#Int32#]{{; name=.+$}}
// CLANG_FOO_HELPER_SUB-DAG: Decl[Struct]/OtherModule:       FooHelperSubEnum1[#FooHelperSubEnum1#]{{; name=.+$}}
// CLANG_FOO_HELPER_SUB-DAG: Decl[GlobalVar]/OtherModule:    FooHelperSubEnum1X[#FooHelperSubEnum1#]{{; name=.+$}}
// CLANG_FOO_HELPER_SUB-DAG: Decl[GlobalVar]/OtherModule:    FooHelperSubEnum1Y[#FooHelperSubEnum1#]{{; name=.+$}}
// CLANG_FOO_HELPER_SUB-DAG: Decl[GlobalVar]/OtherModule:    FooHelperSubUnnamedEnumeratorA1[#Int#]{{; name=.+$}}
// CLANG_FOO_HELPER_SUB: End completions

// CLANG_BAR: Begin completions
// CLANG_BAR-DAG: Decl[FreeFunction]/OtherModule: barFunc1({#(a): Int32#})[#Int32#]{{; name=.+$}}
// CLANG_BAR-DAG: Decl[Class]/OtherModule:        BarForwardDeclaredClass[#BarForwardDeclaredClass#]{{; name=.+$}}
// CLANG_BAR-DAG: Decl[Struct]/OtherModule:       BarForwardDeclaredEnum[#BarForwardDeclaredEnum#]{{; name=.+$}}
// CLANG_BAR-DAG: Decl[GlobalVar]/OtherModule:    BarForwardDeclaredEnumValue[#BarForwardDeclaredEnum#]{{; name=.+$}}
// CLANG_BAR-DAG: Decl[GlobalVar]/OtherModule:    BAR_MACRO_1[#Int32#]{{; name=.+$}}
// CLANG_BAR-DAG: Decl[Struct]/OtherModule:       SomeItemSet[#SomeItemSet#]
// CLANG_BAR-DAG: Decl[TypeAlias]/OtherModule:    SomeEnvironment[#SomeItemSet#]
// CLANG_BAR: End completions

// CLANG_BOTH_FOO_BAR: Begin completions
// CLANG_BOTH_FOO_BAR-DAG: Decl[FreeFunction]/OtherModule: redeclaredInMultipleModulesFunc1({#(a): Int32#})[#Int32#]{{; name=.+$}}
// CLANG_BOTH_FOO_BAR: End completions

// CLANG_QUAL_FOO_NEGATIVE-NOT: bar
// CLANG_QUAL_FOO_NEGATIVE-NOT: Bar
// CLANG_QUAL_FOO_NEGATIVE-NOT: BAR

// CLANG_QUAL_BAR_NEGATIVE-NOT: foo
// CLANG_QUAL_BAR_NEGATIVE-NOT: Foo
// CLANG_QUAL_BAR_NEGATIVE-NOT: FOO

func testClangModule() {
  #^FW_UNQUAL_1^#
}

func testCompleteModuleQualifiedFoo1() {
  Foo.#^CLANG_QUAL_FOO_1^#
}

func testCompleteModuleQualifiedFoo2() {
  Foo#^CLANG_QUAL_FOO_2^#
// If the number of results below changes, then you need to add a result to the
// list below.
// CLANG_QUAL_FOO_2: Begin completions, 62 items
// CLANG_QUAL_FOO_2-DAG: Decl[Class]/OtherModule:        .FooClassBase[#FooClassBase#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[Class]/OtherModule:        .FooClassDerived[#FooClassDerived#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[Class]/OtherModule:        .ClassWithInternalProt[#ClassWithInternalProt#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[Struct]/OtherModule:       ._InternalStruct[#_InternalStruct#]
// CLANG_QUAL_FOO_2-DAG: Decl[Class]/OtherModule:        .FooClassPropertyOwnership[#FooClassPropertyOwnership#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[FreeFunction]/OtherModule: ._internalTopLevelFunc()[#Void#]
// CLANG_QUAL_FOO_2-DAG: Decl[Enum]/OtherModule:         .FooComparisonResult[#FooComparisonResult#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[FreeFunction]/OtherModule: .fooFunc1({#(a): Int32#})[#Int32#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[FreeFunction]/OtherModule: .fooFunc1AnonymousParam({#Int32#})[#Int32#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[FreeFunction]/OtherModule: .fooFunc3({#(a): Int32#}, {#(b): Float#}, {#(c): Double#}, {#(d): UnsafeMutablePointer<Int32>#})[#Int32#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[FreeFunction]/OtherModule: .fooFuncNoreturn1()[#Void#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[FreeFunction]/OtherModule: .fooFuncNoreturn2()[#Void#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[FreeFunction]/OtherModule: .fooFuncWithBlock({#(blk): ((Float) -> Int32)!##(Float) -> Int32#})[#Void#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[FreeFunction]/OtherModule: .fooFuncWithFunctionPointer({#(fptr): CFunctionPointer<((Float) -> Int32)>#})[#Void#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[FreeFunction]/OtherModule: .fooFuncWithComment1()[#Void#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[FreeFunction]/OtherModule: .fooFuncWithComment2()[#Void#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[FreeFunction]/OtherModule: .fooFuncWithComment3()[#Void#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[FreeFunction]/OtherModule: .fooFuncWithComment4()[#Void#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[FreeFunction]/OtherModule: .fooFuncWithComment5()[#Void#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[FreeFunction]/OtherModule: .fooHelperFunc1({#(a): Int32#})[#Int32#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[FreeFunction]/OtherModule: .fooHelperSubFunc1({#(a): Int32#})[#Int32#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[FreeFunction]/OtherModule: .fooSubFunc1({#(a): Int32#})[#Int32#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[FreeFunction]/OtherModule: .redeclaredInMultipleModulesFunc1({#(a): Int32#})[#Int32#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[FreeFunction]/OtherModule: .theLastDeclInFoo()[#Void#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule:    .FOO_MACRO_1[#Int32#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule:    .FOO_MACRO_2[#Int32#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule:    .FOO_MACRO_3[#Int32#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule:    .FOO_MACRO_4[#UInt32#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule:    .FOO_MACRO_5[#UInt64#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule:    .FOO_MACRO_REDEF_1[#Int32#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule:    .FOO_MACRO_REDEF_2[#Int32#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule:    .FooEnum1X[#FooEnum1#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule:    .FooEnum2X[#FooEnum2#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule:    .FooEnum2Y[#FooEnum2#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule:    .FooEnum3X[#FooEnum3#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule:    .FooEnum3Y[#FooEnum3#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule:    .FooHelperSubEnum1X[#FooHelperSubEnum1#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule:    .FooHelperSubEnum1Y[#FooHelperSubEnum1#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule:    .FooHelperSubUnnamedEnumeratorA1[#Int#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule:    .FooHelperUnnamedEnumeratorA1[#Int#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule:    .FooHelperUnnamedEnumeratorA2[#Int#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule:    .FooSubEnum1X[#FooSubEnum1#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule:    .FooSubEnum1Y[#FooSubEnum1#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule:    .FooSubUnnamedEnumeratorA1[#Int#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule:    .fooIntVar[#Int32#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[Protocol]/OtherModule:     ._InternalProt[#_InternalProt#]
// CLANG_QUAL_FOO_2-DAG: Decl[Protocol]/OtherModule:     .FooProtocolBase[#FooProtocolBase#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[Protocol]/OtherModule:     .FooProtocolDerived[#FooProtocolDerived#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[Struct]/OtherModule:       .FooEnum1[#FooEnum1#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[Struct]/OtherModule:       .FooEnum2[#FooEnum2#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[Struct]/OtherModule:       .FooEnum3[#FooEnum3#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[Struct]/OtherModule:       .FooHelperSubEnum1[#FooHelperSubEnum1#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[Struct]/OtherModule:       .FooRuncingOptions[#FooRuncingOptions#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[Struct]/OtherModule:       .FooStruct1[#FooStruct1#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[Struct]/OtherModule:       .FooStruct2[#FooStruct2#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[Struct]/OtherModule:       .FooStructTypedef2[#FooStructTypedef2#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[Struct]/OtherModule:       .FooSubEnum1[#FooSubEnum1#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[TypeAlias]/OtherModule:    .FooStructTypedef1[#FooStruct2#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG:  Decl[Class]/OtherModule:            .FooUnavailableMembers[#FooUnavailableMembers#]
// CLANG_QUAL_FOO_2-DAG: Decl[TypeAlias]/OtherModule:    .FooTypedef1[#Int32#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG:   Decl[TypeAlias]/OtherModule:        .FooCFTypeRef[#FooCFType#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG:  Decl[Class]/OtherModule: .FooRepeatedMembers[#FooRepeatedMembers#]{{; name=.+$}}
// CLANG_QUAL_FOO_2: End completions
}

func testCompleteModuleQualifiedBar1() {
  Bar.#^CLANG_QUAL_BAR_1^#
// If the number of results below changes, this is an indication that you need
// to add a result to the appropriate list.  Do not just bump the number!
// CLANG_QUAL_BAR_1: Begin completions, 8 items
}

func testCompleteFunctionCall1() {
  fooFunc1#^FUNCTION_CALL_1^#
// FUNCTION_CALL_1: Begin completions
// FUNCTION_CALL_1-NEXT: Pattern/ExprSpecific: ({#Int32#})[#Int32#]{{; name=.+$}}
// FUNCTION_CALL_1-NEXT: End completions
}

func testCompleteFunctionCall2() {
  fooFunc1AnonymousParam#^FUNCTION_CALL_2^#
// FUNCTION_CALL_2: Begin completions
// FUNCTION_CALL_2-NEXT: Pattern/ExprSpecific: ({#Int32#})[#Int32#]{{; name=.+$}}
// FUNCTION_CALL_2-NEXT: End completions
}

func testCompleteStructMembers1() {
  FooStruct1#^CLANG_STRUCT_MEMBERS_1^#
// CLANG_STRUCT_MEMBERS_1: Begin completions
// CLANG_STRUCT_MEMBERS_1-NEXT: Decl[Constructor]/CurrNominal: ()[#FooStruct1#]{{; name=.+$}}
// CLANG_STRUCT_MEMBERS_1-NEXT: Decl[Constructor]/CurrNominal: ({#x: Int32#}, {#y: Double#})[#FooStruct1#]{{; name=.+$}}
// CLANG_STRUCT_MEMBERS_1-NEXT: End completions
}

func testCompleteClassMembers1() {
  FooClassBase#^CLANG_CLASS_MEMBERS_1^#
// FIXME: do we want to show curried instance functions for Objective-C classes?
// CLANG_CLASS_MEMBERS_1: Begin completions
// CLANG_CLASS_MEMBERS_1-NEXT: Decl[StaticMethod]/CurrNominal:     .fooBaseInstanceFunc0()[#Void#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_1-NEXT: Decl[InstanceMethod]/CurrNominal:   .fooBaseInstanceFunc0({#self: FooClassBase#})[#() -> Void#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_1-NEXT: Decl[StaticMethod]/CurrNominal:     .fooBaseInstanceFunc1({#(anObject): AnyObject!#})[#FooClassBase!#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_1-NEXT: Decl[InstanceMethod]/CurrNominal:   .fooBaseInstanceFunc1({#self: FooClassBase#})[#(AnyObject!) -> FooClassBase!#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_1-NEXT: Decl[Constructor]/CurrNominal:      ()[#FooClassBase!#]
// CLANG_CLASS_MEMBERS_1-NEXT: Decl[Constructor]/CurrNominal:      ({#float: Float#})[#FooClassBase!#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_1-NEXT: Decl[StaticMethod]/CurrNominal:     .fooBaseInstanceFuncOverridden()[#Void#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_1-NEXT: Decl[InstanceMethod]/CurrNominal:   .fooBaseInstanceFuncOverridden({#self: FooClassBase#})[#() -> Void#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_1-NEXT: Decl[StaticMethod]/CurrNominal:     .fooBaseClassFunc0()[#Void#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_1-NEXT: Decl[StaticMethod]/CurrNominal:     ._internalMeth3()[#AnyObject!#]
// CLANG_CLASS_MEMBERS_1-NEXT: Decl[InstanceMethod]/CurrNominal:   ._internalMeth3({#self: FooClassBase#})[#() -> AnyObject!#]
// CLANG_CLASS_MEMBERS_1-NEXT: Decl[StaticMethod]/CurrNominal:     ._internalMeth2()[#AnyObject!#]
// CLANG_CLASS_MEMBERS_1-NEXT: Decl[InstanceMethod]/CurrNominal:   ._internalMeth2({#self: FooClassBase#})[#() -> AnyObject!#]
// CLANG_CLASS_MEMBERS_1-NEXT: Decl[StaticMethod]/CurrNominal:     .nonInternalMeth()[#AnyObject!#]
// CLANG_CLASS_MEMBERS_1-NEXT: Decl[InstanceMethod]/CurrNominal:   .nonInternalMeth({#self: FooClassBase#})[#() -> AnyObject!#]
// CLANG_CLASS_MEMBERS_1-NEXT: Decl[StaticMethod]/CurrNominal:     ._internalMeth1()[#AnyObject!#]
// CLANG_CLASS_MEMBERS_1-NEXT: Decl[InstanceMethod]/CurrNominal:   ._internalMeth1({#self: FooClassBase#})[#() -> AnyObject!#]
// CLANG_CLASS_MEMBERS_1-NEXT: End completions
}

func testCompleteClassMembers2() {
  FooClassDerived#^CLANG_CLASS_MEMBERS_2^#

// CLANG_CLASS_MEMBERS_2: Begin completions
// CLANG_CLASS_MEMBERS_2-NEXT: Decl[InstanceMethod]/CurrNominal:   .fooInstanceFunc0({#self: FooClassDerived#})[#() -> Void#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_2-NEXT: Decl[InstanceMethod]/CurrNominal:   .fooInstanceFunc1({#self: FooClassDerived#})[#(Int32) -> Void#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_2-NEXT: Decl[InstanceMethod]/CurrNominal:   .fooInstanceFunc2({#self: FooClassDerived#})[#(Int32, withB: Int32) -> Void#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_2-NEXT: Decl[InstanceMethod]/CurrNominal:   .fooBaseInstanceFuncOverridden({#self: FooClassDerived#})[#() -> Void#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_2-NEXT: Decl[StaticMethod]/CurrNominal:     .fooClassFunc0()[#Void#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_2-NEXT: Decl[Constructor]/CurrNominal:      ()[#FooClassDerived!#]
// CLANG_CLASS_MEMBERS_2-NEXT: Decl[Constructor]/CurrNominal:      ({#float: Float#})[#FooClassDerived!#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_2-NEXT: Decl[InstanceMethod]/CurrNominal:   .fooProtoFunc({#self: FooClassDerived#})[#() -> Void#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_2-NEXT: Decl[InstanceMethod]/CurrNominal:   .fooProtoFuncWithExtraIndentation1({#self: FooClassDerived#})[#() -> Void#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_2-NEXT: Decl[InstanceMethod]/CurrNominal:   .fooProtoFuncWithExtraIndentation2({#self: FooClassDerived#})[#() -> Void#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_2-NEXT: Decl[StaticMethod]/CurrNominal:     .fooProtoClassFunc()[#Void#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_2-NEXT: Decl[StaticMethod]/Super:           .fooBaseInstanceFunc0()[#Void#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_2-NEXT: Decl[InstanceMethod]/Super:         .fooBaseInstanceFunc0({#self: FooClassBase#})[#() -> Void#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_2-NEXT: Decl[StaticMethod]/Super:           .fooBaseInstanceFunc1({#(anObject): AnyObject!#})[#FooClassBase!#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_2-NEXT: Decl[InstanceMethod]/Super:         .fooBaseInstanceFunc1({#self: FooClassBase#})[#(AnyObject!) -> FooClassBase!#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_2-NEXT: Decl[StaticMethod]/Super:           .fooBaseInstanceFuncOverridden()[#Void#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_2-NEXT: Decl[StaticMethod]/Super:           .fooBaseClassFunc0()[#Void#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_2-NEXT: Decl[StaticMethod]/Super:           ._internalMeth3()[#AnyObject!#]
// CLANG_CLASS_MEMBERS_2-NEXT: Decl[InstanceMethod]/Super:         ._internalMeth3({#self: FooClassBase#})[#() -> AnyObject!#]
// CLANG_CLASS_MEMBERS_2-NEXT: Decl[StaticMethod]/Super:           ._internalMeth2()[#AnyObject!#]
// CLANG_CLASS_MEMBERS_2-NEXT: Decl[InstanceMethod]/Super:         ._internalMeth2({#self: FooClassBase#})[#() -> AnyObject!#]
// CLANG_CLASS_MEMBERS_2-NEXT: Decl[StaticMethod]/Super:           .nonInternalMeth()[#AnyObject!#]
// CLANG_CLASS_MEMBERS_2-NEXT: Decl[InstanceMethod]/Super:         .nonInternalMeth({#self: FooClassBase#})[#() -> AnyObject!#]
// CLANG_CLASS_MEMBERS_2-NEXT: Decl[StaticMethod]/Super:           ._internalMeth1()[#AnyObject!#]
// CLANG_CLASS_MEMBERS_2-NEXT: Decl[InstanceMethod]/Super:         ._internalMeth1({#self: FooClassBase#})[#() -> AnyObject!#]
// CLANG_CLASS_MEMBERS_2-NEXT: End completions
}

func testCompleteInstanceMembers1(fooObject: FooClassDerived) {
  fooObject#^CLANG_INSTANCE_MEMBERS_1^#
// CLANG_INSTANCE_MEMBERS_1: Begin completions
// CLANG_INSTANCE_MEMBERS_1-NEXT: Decl[InstanceVar]/CurrNominal:      .fooProperty1[#Int32#]{{; name=.+$}}
// CLANG_INSTANCE_MEMBERS_1-NEXT: Decl[InstanceVar]/CurrNominal:      .fooProperty2[#Int32#]{{; name=.+$}}
// CLANG_INSTANCE_MEMBERS_1-NEXT: Decl[InstanceVar]/CurrNominal:      .fooProperty3[#Int32#]{{; name=.+$}}
// CLANG_INSTANCE_MEMBERS_1-NEXT: Decl[InstanceMethod]/CurrNominal:   .fooInstanceFunc0()[#Void#]{{; name=.+$}}
// CLANG_INSTANCE_MEMBERS_1-NEXT: Decl[InstanceMethod]/CurrNominal:   .fooInstanceFunc1({#(a): Int32#})[#Void#]{{; name=.+$}}
// CLANG_INSTANCE_MEMBERS_1-NEXT: Decl[InstanceMethod]/CurrNominal:   .fooInstanceFunc2({#(a): Int32#}, {#withB: Int32#})[#Void#]{{; name=.+$}}
// CLANG_INSTANCE_MEMBERS_1-NEXT: Decl[InstanceMethod]/CurrNominal:   .fooBaseInstanceFuncOverridden()[#Void#]{{; name=.+$}}
// CLANG_INSTANCE_MEMBERS_1-NEXT: Decl[InstanceMethod]/CurrNominal:   .fooProtoFunc()[#Void#]{{; name=.+$}}
// CLANG_INSTANCE_MEMBERS_1-NEXT: Decl[InstanceMethod]/CurrNominal:   .fooProtoFuncWithExtraIndentation1()[#Void#]{{; name=.+$}}
// CLANG_INSTANCE_MEMBERS_1-NEXT: Decl[InstanceMethod]/CurrNominal:   .fooProtoFuncWithExtraIndentation2()[#Void#]{{; name=.+$}}
// CLANG_INSTANCE_MEMBERS_1-NEXT: Decl[InstanceMethod]/Super:         .fooBaseInstanceFunc0()[#Void#]{{; name=.+$}}
// CLANG_INSTANCE_MEMBERS_1-NEXT: Decl[InstanceMethod]/Super:         .fooBaseInstanceFunc1({#(anObject): AnyObject!#})[#FooClassBase!#]{{; name=.+$}}
// CLANG_INSTANCE_MEMBERS_1-NEXT: Decl[InstanceMethod]/Super:         ._internalMeth3()[#AnyObject!#]
// CLANG_INSTANCE_MEMBERS_1-NEXT: Decl[InstanceMethod]/Super:         ._internalMeth2()[#AnyObject!#]
// CLANG_INSTANCE_MEMBERS_1-NEXT: Decl[InstanceMethod]/Super:         .nonInternalMeth()[#AnyObject!#]
// CLANG_INSTANCE_MEMBERS_1-NEXT: Decl[InstanceMethod]/Super:         ._internalMeth1()[#AnyObject!#]
// CLANG_INSTANCE_MEMBERS_1-NEXT: End completions
}

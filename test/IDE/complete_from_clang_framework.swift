// RUN: %batch-code-completion -F %S/Inputs/mock-sdk -enable-objc-interop

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
// SWIFT_COMPLETIONS-DAG: Keyword[self]/CurrNominal: self[#SwiftStruct#]; name=self
// SWIFT_COMPLETIONS-DAG: Decl[InstanceVar]/CurrNominal: instanceVar[#Int#]{{; name=.+$}}
}

// CLANG_FOO-DAG: Decl[Struct]/OtherModule[Foo]:       FooEnum1[#FooEnum1#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[GlobalVar]/OtherModule[Foo]:    FooEnum1X[#FooEnum1#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[Struct]/OtherModule[Foo]:       FooEnum2[#FooEnum2#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[GlobalVar]/OtherModule[Foo]:    FooEnum2X[#FooEnum2#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[GlobalVar]/OtherModule[Foo]:    FooEnum2Y[#FooEnum2#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[Struct]/OtherModule[Foo]:       FooEnum3[#FooEnum3#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[GlobalVar]/OtherModule[Foo]:    FooEnum3X[#FooEnum3#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[GlobalVar]/OtherModule[Foo]:    FooEnum3Y[#FooEnum3#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[Enum]/OtherModule[Foo]:         FooComparisonResult[#FooComparisonResult#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[Struct]/OtherModule[Foo]:       FooRuncingOptions[#FooRuncingOptions#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[Struct]/OtherModule[Foo]:       FooStruct1[#FooStruct1#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[Struct]/OtherModule[Foo]:       FooStruct2[#FooStruct2#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[TypeAlias]/OtherModule[Foo]:    FooStructTypedef1[#FooStruct2#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[Struct]/OtherModule[Foo]:       FooStructTypedef2[#FooStructTypedef2#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[TypeAlias]/OtherModule[Foo]:    FooTypedef1[#Int32#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[GlobalVar]/OtherModule[Foo]:    fooIntVar[#Int32#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[FreeFunction]/OtherModule[Foo]: fooFunc1({#(a): Int32#})[#Int32#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[FreeFunction]/OtherModule[Foo]: fooFunc1AnonymousParam({#Int32#})[#Int32#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[FreeFunction]/OtherModule[Foo]: fooFunc3({#(a): Int32#}, {#(b): Float#}, {#(c): Double#}, {#(d): UnsafeMutablePointer<Int32>!#})[#Int32#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[FreeFunction]/OtherModule[Foo]: fooFuncWithBlock({#(blk): ((Float) -> Int32)!##(Float) -> Int32#})[#Void#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[FreeFunction]/OtherModule[Foo]: fooFuncWithComment1()[#Void#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[FreeFunction]/OtherModule[Foo]: fooFuncWithComment2()[#Void#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[FreeFunction]/OtherModule[Foo]: fooFuncWithComment3()[#Void#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[FreeFunction]/OtherModule[Foo]: fooFuncWithComment4()[#Void#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[FreeFunction]/OtherModule[Foo]: fooFuncWithComment5()[#Void#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[Protocol]/OtherModule[Foo]/Flair[RareType]: FooProtocolBase[#FooProtocolBase#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[Protocol]/OtherModule[Foo]/Flair[RareType]: FooProtocolDerived[#FooProtocolDerived#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[Class]/OtherModule[Foo]:        FooClassBase[#FooClassBase#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[Class]/OtherModule[Foo]:        FooClassDerived[#FooClassDerived#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[GlobalVar]/OtherModule[Foo]:    FOO_MACRO_1[#Int32#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[GlobalVar]/OtherModule[Foo]:    FOO_MACRO_2[#Int32#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[GlobalVar]/OtherModule[Foo]:    FOO_MACRO_3[#Int32#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[GlobalVar]/OtherModule[Foo]:    FOO_MACRO_4[#UInt32#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[GlobalVar]/OtherModule[Foo]:    FOO_MACRO_5[#UInt64#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[GlobalVar]/OtherModule[Foo]:    FOO_MACRO_REDEF_1[#Int32#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[GlobalVar]/OtherModule[Foo]:    FOO_MACRO_REDEF_2[#Int32#]{{; name=.+$}}
// CLANG_FOO-DAG: Decl[FreeFunction]/OtherModule[Foo]: theLastDeclInFoo()[#Void#]{{; name=.+$}}

// CLANG_FOO_SUB-DAG: Decl[FreeFunction]/OtherModule[Foo.FooSub]: fooSubFunc1({#(a): Int32#})[#Int32#]{{; name=.+$}}
// CLANG_FOO_SUB-DAG: Decl[Struct]/OtherModule[Foo.FooSub]:       FooSubEnum1[#FooSubEnum1#]{{; name=.+$}}
// CLANG_FOO_SUB-DAG: Decl[GlobalVar]/OtherModule[Foo.FooSub]:    FooSubEnum1X[#FooSubEnum1#]{{; name=.+$}}
// CLANG_FOO_SUB-DAG: Decl[GlobalVar]/OtherModule[Foo.FooSub]:    FooSubEnum1Y[#FooSubEnum1#]{{; name=.+$}}
// CLANG_FOO_SUB-DAG: Decl[GlobalVar]/OtherModule[Foo.FooSub]:    FooSubUnnamedEnumeratorA1[#Int#]{{; name=.+$}}

// CLANG_FOO_HELPER-DAG: Decl[FreeFunction]/OtherModule[FooHelper]: fooHelperFunc1({#(a): Int32#})[#Int32#]{{; name=.+$}}
// CLANG_FOO_HELPER-DAG: Decl[GlobalVar]/OtherModule[FooHelper]:    FooHelperUnnamedEnumeratorA1[#Int#]{{; name=.+$}}
// CLANG_FOO_HELPER-DAG: Decl[GlobalVar]/OtherModule[FooHelper]:    FooHelperUnnamedEnumeratorA2[#Int#]{{; name=.+$}}

// CLANG_FOO_HELPER_SUB-DAG: Decl[FreeFunction]/OtherModule[FooHelper.FooHelperSub]: fooHelperSubFunc1({#(a): Int32#})[#Int32#]{{; name=.+$}}
// CLANG_FOO_HELPER_SUB-DAG: Decl[Struct]/OtherModule[FooHelper.FooHelperSub]:       FooHelperSubEnum1[#FooHelperSubEnum1#]{{; name=.+$}}
// CLANG_FOO_HELPER_SUB-DAG: Decl[GlobalVar]/OtherModule[FooHelper.FooHelperSub]:    FooHelperSubEnum1X[#FooHelperSubEnum1#]{{; name=.+$}}
// CLANG_FOO_HELPER_SUB-DAG: Decl[GlobalVar]/OtherModule[FooHelper.FooHelperSub]:    FooHelperSubEnum1Y[#FooHelperSubEnum1#]{{; name=.+$}}
// CLANG_FOO_HELPER_SUB-DAG: Decl[GlobalVar]/OtherModule[FooHelper.FooHelperSub]:    FooHelperSubUnnamedEnumeratorA1[#Int#]{{; name=.+$}}

// CLANG_BAR-DAG: Decl[FreeFunction]/OtherModule[Bar]: barFunc1({#(a): Int32#})[#Int32#]{{; name=.+$}}
// CLANG_BAR-DAG: Decl[Class]/OtherModule[Bar]:        BarForwardDeclaredClass[#BarForwardDeclaredClass#]{{; name=.+$}}
// CLANG_BAR-DAG: Decl[Struct]/OtherModule[Bar]:       BarForwardDeclaredEnum[#BarForwardDeclaredEnum#]{{; name=.+$}}
// CLANG_BAR-DAG: Decl[GlobalVar]/OtherModule[Bar]:    BarForwardDeclaredEnumValue[#BarForwardDeclaredEnum#]{{; name=.+$}}
// CLANG_BAR-DAG: Decl[GlobalVar]/OtherModule[Bar]:    BAR_MACRO_1[#Int32#]{{; name=.+$}}
// CLANG_BAR-DAG: Decl[Struct]/OtherModule[Bar]:       SomeItemSet[#SomeItemSet#]
// CLANG_BAR-DAG: Decl[TypeAlias]/OtherModule[Bar]:    SomeEnvironment[#SomeItemSet#]

// CLANG_BOTH_FOO_BAR-DAG: Decl[FreeFunction]/OtherModule[{{(Foo|Bar)}}]: redeclaredInMultipleModulesFunc1({#(a): Int32#})[#Int32#]{{; name=.+$}}

// CLANG_QUAL_FOO_NEGATIVE-NOT: bar
// CLANG_QUAL_FOO_NEGATIVE-NOT: :{{.*}}Bar
// CLANG_QUAL_FOO_NEGATIVE-NOT: BAR

// CLANG_QUAL_BAR_NEGATIVE-NOT: foo
// CLANG_QUAL_BAR_NEGATIVE-NOT: :{{.*}}Foo
// CLANG_QUAL_BAR_NEGATIVE-NOT: FOO

func testClangModule() {
  #^FW_UNQUAL_1?check=CLANG_FOO;check=CLANG_FOO_SUB;check=CLANG_FOO_HELPER;check=CLANG_FOO_HELPER_SUB;check=CLANG_BAR;check=CLANG_BOTH_FOO_BAR^#
}

func testCompleteModuleQualifiedFoo1() {
  Foo.#^CLANG_QUAL_FOO_1?check=CLANG_FOO;check=CLANG_FOO_SUB;check=CLANG_QUAL_FOO_NEGATIVE^#
}

func testCompleteModuleQualifiedFoo2() {
  Foo#^CLANG_QUAL_FOO_2^#
// If the number of results below changes, then you need to add a result to the
// list below.
// CLANG_QUAL_FOO_2: Begin completions, 76 items
// CLANG_QUAL_FOO_2-DAG: Decl[Class]/OtherModule[Foo]:        .FooClassBase[#FooClassBase#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[Class]/OtherModule[Foo]:        .FooClassDerived[#FooClassDerived#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[Class]/OtherModule[Foo]:        .ClassWithInternalProt[#ClassWithInternalProt#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[Struct]/OtherModule[Foo]:       ._InternalStruct[#_InternalStruct#]
// CLANG_QUAL_FOO_2-DAG: Decl[Class]/OtherModule[Foo]:        .FooClassPropertyOwnership[#FooClassPropertyOwnership#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[FreeFunction]/OtherModule[Foo]: ._internalTopLevelFunc()[#Void#]
// CLANG_QUAL_FOO_2-DAG: Decl[Enum]/OtherModule[Foo]:         .FooComparisonResult[#FooComparisonResult#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[FreeFunction]/OtherModule[Foo]: .fooFunc1({#(a): Int32#})[#Int32#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[FreeFunction]/OtherModule[Foo]: .fooFunc1AnonymousParam({#Int32#})[#Int32#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[FreeFunction]/OtherModule[Foo]: .fooFunc3({#(a): Int32#}, {#(b): Float#}, {#(c): Double#}, {#(d): UnsafeMutablePointer<Int32>!#})[#Int32#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[FreeFunction]/OtherModule[Foo]: .fooFuncNoreturn1()[#Never#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[FreeFunction]/OtherModule[Foo]: .fooFuncNoreturn2()[#Never#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[FreeFunction]/OtherModule[Foo]: .fooFuncWithBlock({#(blk): ((Float) -> Int32)!##(Float) -> Int32#})[#Void#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[FreeFunction]/OtherModule[Foo]: .fooFuncWithFunctionPointer({#(fptr): ((Float) -> Int32)!##(Float) -> Int32#})[#Void#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[FreeFunction]/OtherModule[Foo]: .fooFuncWithComment1()[#Void#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[FreeFunction]/OtherModule[Foo]: .fooFuncWithComment2()[#Void#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[FreeFunction]/OtherModule[Foo]: .fooFuncWithComment3()[#Void#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[FreeFunction]/OtherModule[Foo]: .fooFuncWithComment4()[#Void#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[FreeFunction]/OtherModule[Foo]: .fooFuncWithComment5()[#Void#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[FreeFunction]/OtherModule[FooHelper]: .fooHelperFunc1({#(a): Int32#})[#Int32#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[FreeFunction]/OtherModule[FooHelper.FooHelperSub]: .fooHelperSubFunc1({#(a): Int32#})[#Int32#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[FreeFunction]/OtherModule[Foo.FooSub]: .fooSubFunc1({#(a): Int32#})[#Int32#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[FreeFunction]/OtherModule[{{(Foo|Bar)}}]: .redeclaredInMultipleModulesFunc1({#(a): Int32#})[#Int32#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[FreeFunction]/OtherModule[Foo]: .theLastDeclInFoo()[#Void#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule[Foo]:    .FOO_MACRO_1[#Int32#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule[Foo]:    .FOO_MACRO_2[#Int32#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule[Foo]:    .FOO_MACRO_3[#Int32#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule[Foo]:    .FOO_MACRO_4[#UInt32#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule[Foo]:    .FOO_MACRO_5[#UInt64#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule[Foo]:    .FOO_MACRO_6[#typedef_int_t#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule[Foo]:    .FOO_MACRO_7[#typedef_int_t#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule[Foo]:    .FOO_MACRO_OR[#Int32#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule[Foo]:    .FOO_MACRO_AND[#Int32#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule[Foo]:    .FOO_MACRO_REDEF_1[#Int32#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule[Foo]:    .FOO_MACRO_REDEF_2[#Int32#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule[Foo]:    .FooEnum1X[#FooEnum1#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule[Foo]:    .FooEnum2X[#FooEnum2#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule[Foo]:    .FooEnum2Y[#FooEnum2#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule[Foo]:    .FooEnum3X[#FooEnum3#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule[Foo]:    .FooEnum3Y[#FooEnum3#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule[FooHelper.FooHelperSub]: .FooHelperSubEnum1X[#FooHelperSubEnum1#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule[FooHelper.FooHelperSub]: .FooHelperSubEnum1Y[#FooHelperSubEnum1#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule[FooHelper.FooHelperSub]: .FooHelperSubUnnamedEnumeratorA1[#Int#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule[FooHelper]:  .FooHelperUnnamedEnumeratorA1[#Int#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule[FooHelper]:  .FooHelperUnnamedEnumeratorA2[#Int#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule[Foo.FooSub]: .FooSubEnum1X[#FooSubEnum1#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule[Foo.FooSub]: .FooSubEnum1Y[#FooSubEnum1#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule[Foo.FooSub]: .FooSubUnnamedEnumeratorA1[#Int#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[GlobalVar]/OtherModule[Foo]:     .fooIntVar[#Int32#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[Protocol]/OtherModule[Foo]/Flair[RareType]: ._InternalProt[#_InternalProt#]
// CLANG_QUAL_FOO_2-DAG: Decl[Protocol]/OtherModule[Foo]/Flair[RareType]: .FooProtocolBase[#FooProtocolBase#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[Protocol]/OtherModule[Foo]/Flair[RareType]: .FooProtocolDerived[#FooProtocolDerived#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[Struct]/OtherModule[Foo]:        .FooEnum1[#FooEnum1#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[Struct]/OtherModule[Foo]:        .FooEnum2[#FooEnum2#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[Struct]/OtherModule[Foo]:        .FooEnum3[#FooEnum3#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[Struct]/OtherModule[FooHelper.FooHelperSub]: .FooHelperSubEnum1[#FooHelperSubEnum1#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[Struct]/OtherModule[Foo]:        .FooRuncingOptions[#FooRuncingOptions#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[Struct]/OtherModule[Foo]:        .FooStruct1[#FooStruct1#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[Struct]/OtherModule[Foo]:        .FooStruct2[#FooStruct2#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[Struct]/OtherModule[Foo]:        .FooStructTypedef2[#FooStructTypedef2#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[Struct]/OtherModule[Foo.FooSub]: .FooSubEnum1[#FooSubEnum1#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[TypeAlias]/OtherModule[Foo]:     .FooStructTypedef1[#FooStruct2#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG:  Decl[Class]/OtherModule[Foo]:        .FooUnavailableMembers[#FooUnavailableMembers#]
// CLANG_QUAL_FOO_2-DAG: Decl[TypeAlias]/OtherModule[Foo]:     .FooTypedef1[#Int32#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG:   Decl[Class]/OtherModule[Foo]:   .FooCFType[#FooCFType#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG:  Decl[Class]/OtherModule[Foo]:        .FooRepeatedMembers[#FooRepeatedMembers#]{{; name=.+$}}
// CLANG_QUAL_FOO_2-DAG: Decl[Class]/OtherModule[Foo]: .FooClassWithClassProperties[#FooClassWithClassProperties#];
// CLANG_QUAL_FOO_2-DAG: Decl[Enum]/OtherModule[Foo]: .SCNFilterMode[#SCNFilterMode#];
}

func testCompleteModuleQualifiedBar1() {
  Bar.#^CLANG_QUAL_BAR_1?check=CLANG_QUAL_BAR_1;check=CLANG_BAR;check=CLANG_QUAL_BAR_NEGATIVE^#
// If the number of results below changes, this is an indication that you need
// to add a result to the appropriate list.  Do not just bump the number!
// CLANG_QUAL_BAR_1: Begin completions, 8 items
}

func testCompleteFunctionCall1() {
  fooFunc1#^FUNCTION_CALL_1^#
// FUNCTION_CALL_1-DAG: Decl[FreeFunction]/OtherModule[Foo]/Flair[ArgLabels]: ({#(a): Int32#})[#Int32#]{{; name=.+$}}
// FUNCTION_CALL_1-DAG: Keyword[self]/CurrNominal: .self[#(Int32) -> Int32#]; name=self
}

func testCompleteFunctionCall2() {
  fooFunc1AnonymousParam#^FUNCTION_CALL_2^#
// FUNCTION_CALL_2-DAG: Decl[FreeFunction]/OtherModule[Foo]/Flair[ArgLabels]: ({#Int32#})[#Int32#]{{; name=.+$}}
// FUNCTION_CALL_2-DAG: Keyword[self]/CurrNominal: .self[#(Int32) -> Int32#]; name=self
}

func testCompleteStructMembers1() {
  FooStruct1#^CLANG_STRUCT_MEMBERS_1^#
// CLANG_STRUCT_MEMBERS_1-DAG: Decl[Constructor]/CurrNominal/Flair[ArgLabels]: ()[#FooStruct1#]{{; name=.+$}}
// CLANG_STRUCT_MEMBERS_1-DAG: Decl[Constructor]/CurrNominal/Flair[ArgLabels]: ({#x: Int32#}, {#y: Double#})[#FooStruct1#]{{; name=.+$}}
// CLANG_STRUCT_MEMBERS_1-DAG: Keyword[self]/CurrNominal: .self[#FooStruct1.Type#]; name=self
// CLANG_STRUCT_MEMBERS_1-DAG: Keyword/CurrNominal: .Type[#FooStruct1.Type#]; name=Type
}

func testCompleteClassMembers1() {
  FooClassBase#^CLANG_CLASS_MEMBERS_1^#
// FIXME: do we want to show curried instance functions for Objective-C classes?
// CLANG_CLASS_MEMBERS_1-DAG: Decl[StaticMethod]/CurrNominal:     .fooBaseInstanceFunc0()[#Void#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_1-DAG: Decl[InstanceMethod]/CurrNominal:   .fooBaseInstanceFunc0({#(self): FooClassBase#})[#() -> Void#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_1-DAG: Decl[StaticMethod]/CurrNominal:     .fooBaseInstanceFunc1({#(anObject): Any!#})[#FooClassBase!#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_1-DAG: Decl[InstanceMethod]/CurrNominal:   .fooBaseInstanceFunc1({#(self): FooClassBase#})[#(Any?) -> FooClassBase?#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_1-DAG: Decl[Constructor]/CurrNominal/Flair[ArgLabels]:      ()[#FooClassBase!#]
// CLANG_CLASS_MEMBERS_1-DAG: Decl[Constructor]/CurrNominal/Flair[ArgLabels]:      ({#float: Float#})[#FooClassBase!#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_1-DAG: Decl[StaticMethod]/CurrNominal:     .fooBaseInstanceFuncOverridden()[#Void#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_1-DAG: Decl[InstanceMethod]/CurrNominal:   .fooBaseInstanceFuncOverridden({#(self): FooClassBase#})[#() -> Void#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_1-DAG: Decl[StaticMethod]/CurrNominal:     .fooBaseClassFunc0()[#Void#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_1-DAG: Decl[Constructor]/CurrNominal/Flair[ArgLabels]:      ({#(x): Int32#})[#FooClassBase!#]
// CLANG_CLASS_MEMBERS_1-DAG: Decl[StaticMethod]/CurrNominal:     ._internalMeth3()[#Any!#]
// CLANG_CLASS_MEMBERS_1-DAG: Decl[InstanceMethod]/CurrNominal:   ._internalMeth3({#(self): FooClassBase#})[#() -> Any?#]
// CLANG_CLASS_MEMBERS_1-DAG: Decl[StaticMethod]/CurrNominal:     ._internalMeth2()[#Any!#]
// CLANG_CLASS_MEMBERS_1-DAG: Decl[InstanceMethod]/CurrNominal:   ._internalMeth2({#(self): FooClassBase#})[#() -> Any?#]
// CLANG_CLASS_MEMBERS_1-DAG: Decl[StaticMethod]/CurrNominal:     .nonInternalMeth()[#Any!#]
// CLANG_CLASS_MEMBERS_1-DAG: Decl[InstanceMethod]/CurrNominal:   .nonInternalMeth({#(self): FooClassBase#})[#() -> Any?#]
// CLANG_CLASS_MEMBERS_1-DAG: Decl[StaticMethod]/CurrNominal:     ._internalMeth1()[#Any!#]
// CLANG_CLASS_MEMBERS_1-DAG: Decl[InstanceMethod]/CurrNominal:   ._internalMeth1({#(self): FooClassBase#})[#() -> Any?#]
// CLANG_CLASS_MEMBERS_1-DAG: Keyword[self]/CurrNominal: .self[#FooClassBase.Type#]; name=self
// CLANG_CLASS_MEMBERS_1-DAG: Keyword/CurrNominal: .Type[#FooClassBase.Type#]; name=Type
}

func testCompleteClassMembers2() {
  FooClassDerived#^CLANG_CLASS_MEMBERS_2^#

// CLANG_CLASS_MEMBERS_2-DAG: Decl[InstanceMethod]/CurrNominal:   .fooInstanceFunc0({#(self): FooClassDerived#})[#() -> Void#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_2-DAG: Decl[InstanceMethod]/CurrNominal:   .fooInstanceFunc1({#(self): FooClassDerived#})[#(Int32) -> Void#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_2-DAG: Decl[InstanceMethod]/CurrNominal:   .fooInstanceFunc2({#(self): FooClassDerived#})[#(Int32, withB: Int32) -> Void#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_2-DAG: Decl[InstanceMethod]/CurrNominal:   .fooBaseInstanceFuncOverridden({#(self): FooClassDerived#})[#() -> Void#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_2-DAG: Decl[StaticMethod]/CurrNominal:     .fooClassFunc0()[#Void#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_2-DAG: Decl[Constructor]/CurrNominal/Flair[ArgLabels]:      ()[#FooClassDerived!#]
// CLANG_CLASS_MEMBERS_2-DAG: Decl[Constructor]/CurrNominal/Flair[ArgLabels]:      ({#float: Float#})[#FooClassDerived!#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_2-DAG: Decl[InstanceMethod]/CurrNominal:   .fooProtoFunc({#(self): FooClassDerived#})[#() -> Void#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_2-DAG: Decl[InstanceMethod]/CurrNominal:   .fooProtoFuncWithExtraIndentation1({#(self): FooClassDerived#})[#() -> Void#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_2-DAG: Decl[InstanceMethod]/CurrNominal:   .fooProtoFuncWithExtraIndentation2({#(self): FooClassDerived#})[#() -> Void#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_2-DAG: Decl[StaticMethod]/CurrNominal:     .fooProtoClassFunc()[#Void#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_2-DAG: Decl[StaticMethod]/Super:           .fooBaseInstanceFunc0()[#Void#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_2-DAG: Decl[InstanceMethod]/Super:         .fooBaseInstanceFunc0({#(self): FooClassBase#})[#() -> Void#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_2-DAG: Decl[StaticMethod]/Super:           .fooBaseInstanceFunc1({#(anObject): Any!#})[#FooClassBase!#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_2-DAG: Decl[InstanceMethod]/Super:         .fooBaseInstanceFunc1({#(self): FooClassBase#})[#(Any?) -> FooClassBase?#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_2-DAG: Decl[StaticMethod]/Super:           .fooBaseInstanceFuncOverridden()[#Void#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_2-DAG: Decl[StaticMethod]/Super:           .fooBaseClassFunc0()[#Void#]{{; name=.+$}}
// CLANG_CLASS_MEMBERS_2-DAG: Decl[StaticMethod]/Super:           ._internalMeth3()[#Any!#]
// CLANG_CLASS_MEMBERS_2-DAG: Decl[InstanceMethod]/Super:         ._internalMeth3({#(self): FooClassBase#})[#() -> Any?#]
// CLANG_CLASS_MEMBERS_2-DAG: Decl[StaticMethod]/Super:           ._internalMeth2()[#Any!#]
// CLANG_CLASS_MEMBERS_2-DAG: Decl[InstanceMethod]/Super:         ._internalMeth2({#(self): FooClassBase#})[#() -> Any?#]
// CLANG_CLASS_MEMBERS_2-DAG: Decl[StaticMethod]/Super:           .nonInternalMeth()[#Any!#]
// CLANG_CLASS_MEMBERS_2-DAG: Decl[InstanceMethod]/Super:         .nonInternalMeth({#(self): FooClassBase#})[#() -> Any?#]
// CLANG_CLASS_MEMBERS_2-DAG: Decl[StaticMethod]/Super:           ._internalMeth1()[#Any!#]
// CLANG_CLASS_MEMBERS_2-DAG: Decl[InstanceMethod]/Super:         ._internalMeth1({#(self): FooClassBase#})[#() -> Any?#]
// CLANG_CLASS_MEMBERS_2-DAG: Keyword[self]/CurrNominal: .self[#FooClassDerived.Type#]; name=self
// CLANG_CLASS_MEMBERS_2-DAG: Keyword/CurrNominal: .Type[#FooClassDerived.Type#]; name=Type
}

func testCompleteInstanceMembers1(fooObject: FooClassDerived) {
  fooObject#^CLANG_INSTANCE_MEMBERS_1^#
// CLANG_INSTANCE_MEMBERS_1-NOT: Instance
// CLANG_INSTANCE_MEMBERS_1-DAG: Decl[InstanceVar]/CurrNominal:      .fooProperty1[#Int32#]{{; name=.+$}}
// CLANG_INSTANCE_MEMBERS_1-DAG: Decl[InstanceVar]/CurrNominal:      .fooProperty2[#Int32#]{{; name=.+$}}
// CLANG_INSTANCE_MEMBERS_1-DAG: Decl[InstanceVar]/CurrNominal:      .fooProperty3[#Int32#]{{; name=.+$}}
// CLANG_INSTANCE_MEMBERS_1-DAG: Decl[InstanceMethod]/CurrNominal:   .fooInstanceFunc0()[#Void#]{{; name=.+$}}
// CLANG_INSTANCE_MEMBERS_1-DAG: Decl[InstanceMethod]/CurrNominal:   .fooInstanceFunc1({#(a): Int32#})[#Void#]{{; name=.+$}}
// CLANG_INSTANCE_MEMBERS_1-DAG: Decl[InstanceMethod]/CurrNominal:   .fooInstanceFunc2({#(a): Int32#}, {#withB: Int32#})[#Void#]{{; name=.+$}}
// CLANG_INSTANCE_MEMBERS_1-DAG: Decl[InstanceMethod]/CurrNominal:   .fooBaseInstanceFuncOverridden()[#Void#]{{; name=.+$}}
// CLANG_INSTANCE_MEMBERS_1-DAG: Decl[InstanceMethod]/CurrNominal:   .fooProtoFunc()[#Void#]{{; name=.+$}}
// CLANG_INSTANCE_MEMBERS_1-DAG: Decl[InstanceMethod]/CurrNominal:   .fooProtoFuncWithExtraIndentation1()[#Void#]{{; name=.+$}}
// CLANG_INSTANCE_MEMBERS_1-DAG: Decl[InstanceMethod]/CurrNominal:   .fooProtoFuncWithExtraIndentation2()[#Void#]{{; name=.+$}}
// CLANG_INSTANCE_MEMBERS_1-DAG: Decl[InstanceMethod]/Super:         .fooBaseInstanceFunc0()[#Void#]{{; name=.+$}}
// CLANG_INSTANCE_MEMBERS_1-DAG: Decl[InstanceMethod]/Super:         .fooBaseInstanceFunc1({#(anObject): Any!#})[#FooClassBase!#]{{; name=.+$}}
// CLANG_INSTANCE_MEMBERS_1-DAG: Decl[InstanceMethod]/Super:         ._internalMeth3()[#Any!#]
// CLANG_INSTANCE_MEMBERS_1-DAG: Decl[InstanceMethod]/Super:         ._internalMeth2()[#Any!#]
// CLANG_INSTANCE_MEMBERS_1-DAG: Decl[InstanceMethod]/Super:         .nonInternalMeth()[#Any!#]
// CLANG_INSTANCE_MEMBERS_1-DAG: Decl[InstanceMethod]/Super:         ._internalMeth1()[#Any!#]
// CLANG_INSTANCE_MEMBERS_1-NOT: Instance
}

// Check the FooHelper module is suggested even though it's not imported directly
func testExportedModuleCompletion() -> #^TYPE_MODULE_QUALIFIER?check=MODULE_QUALIFIER^# {
  let x = #^EXPR_MODULE_QUALIFIER?check=MODULE_QUALIFIER^#
// MODULE_QUALIFIER-DAG: Decl[Module]/None: swift_ide_test[#Module#]; name=swift_ide_test
// MODULE_QUALIFIER-DAG: Decl[Module]/None/IsSystem: Swift[#Module#]; name=Swift
// MODULE_QUALIFIER-DAG: Decl[Module]/None: Foo[#Module#]; name=Foo
// MODULE_QUALIFIER-DAG: Decl[Module]/None: FooHelper[#Module#]; name=FooHelper
// MODULE_QUALIFIER-DAG: Decl[Module]/None: Bar[#Module#]; name=Bar
}

// %empty-directory(%t)
// %empty-directory(%t.ccp)

// Repeat twice to ensure the cache file is not affected by the flair.
// RUN: %target-swift-ide-test -batch-code-completion -source-filename %s -parse-as-library -filecheck %raw-FileCheck -completion-output-dir %t -completion-cache-path=%t.ccp
// RUN: %target-swift-ide-test -batch-code-completion -source-filename %s -parse-as-library -filecheck %raw-FileCheck -completion-output-dir %t -completion-cache-path=%t.ccp

struct MyStruct {}
protocol MyProtocol {}

func testType() {
    let a: #^TYPE^#
// TYPE-NOT: Keyword[import]
// TYPE-NOT: Keyword[struct]
// TYPE-NOT: Keyword[defer]
// TYPE-DAG: Decl[Struct]/CurrModule: MyStruct[#MyStruct#]; name=MyStruct
// TYPE-DAG: Decl[Protocol]/CurrModule: MyProtocol[#MyProtocol#]; name=MyProtocol
// TYPE-DAG: Decl[Protocol]/OtherModule[Swift]/IsSystem: IteratorProtocol[#IteratorProtocol#]; name=IteratorProtocol
// TYPE-DAG: Decl[Struct]/OtherModule[Swift]/IsSystem: Int[#Int#]; name=Int
}

func testType() {
    #^EXPR^#
// EXPR-DAG: Keyword[import]/None/Flair[RareKeyword]: import; name=import
// EXPR-DAG: Keyword[struct]/None/Flair[RareKeyword]: struct; name=struct
// EXPR-DAG: Keyword[defer]/None: defer; name=defer
// EXPR-DAG: Decl[Struct]/CurrModule: MyStruct[#MyStruct#]; name=MyStruct
// EXPR-DAG: Decl[Protocol]/CurrModule/Flair[RareType]: MyProtocol[#MyProtocol#]; name=MyProtocol
// EXPR-DAG: Decl[Protocol]/OtherModule[Swift]/Flair[RareType]/IsSystem: IteratorProtocol[#IteratorProtocol#]; name=IteratorProtocol
// EXPR-DAG: Decl[Struct]/OtherModule[Swift]/IsSystem: Int[#Int#]; name=Int
}

#^TOPLEVEL^#
// TOPLEVEL-DAG: Keyword[import]/None:       import; name=import
// TOPLEVEL-DAG: Keyword[struct]/None/Flair[CommonKeyword]: struct; name=struct
// TOPLEVEL-DAG: Keyword[defer]/None/Flair[ExprAtFileScope]: defer; name=defer
// TOPLEVEL-DAG: Decl[Struct]/CurrModule/Flair[ExprAtFileScope]: MyStruct[#MyStruct#]; name=MyStruct
// TOPLEVEL-DAG: Decl[Protocol]/CurrModule/Flair[RareType,ExprAtFileScope]: MyProtocol[#MyProtocol#]; name=MyProtocol
// TOPLEVEL-DAG: Decl[Protocol]/OtherModule[Swift]/Flair[RareType,ExprAtFileScope]/IsSystem: IteratorProtocol[#IteratorProtocol#]; name=IteratorProtocol
// TOPLEVEL-DAG: Decl[Struct]/OtherModule[Swift]/Flair[ExprAtFileScope]/IsSystem: Int[#Int#]; name=Int

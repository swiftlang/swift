#^A^#

// Make sure we can code complete at the beginning of the file.
// rdar://14585108
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=A | %FileCheck %s -check-prefix=A
// A-DAG: Keyword[func]/None: func{{; name=.+$}}
// A-DAG: Literal[Boolean]/None: true[#Bool#]{{; name=.+$}}
// A-DAG: Literal[Boolean]/None: false[#Bool#]{{; name=.+$}}
// A-DAG: Literal[Nil]/None: nil{{; name=.+$}}
// A-DAG: Decl[Struct]/OtherModule[Swift]/IsSystem: Int8[#Int8#]{{; name=.+$}}
// A-DAG: Decl[Struct]/OtherModule[Swift]/IsSystem: Int16[#Int16#]{{; name=.+$}}
// A-DAG: Decl[Struct]/OtherModule[Swift]/IsSystem: Int32[#Int32#]{{; name=.+$}}
// A-DAG: Decl[Struct]/OtherModule[Swift]/IsSystem: Int64[#Int64#]{{; name=.+$}}
// A-DAG: Decl[Struct]/OtherModule[Swift]/IsSystem: Bool[#Bool#]{{; name=.+$}}

// This function just adds more non-comment tokens to ensure that the file is not empty.
func foo() {}

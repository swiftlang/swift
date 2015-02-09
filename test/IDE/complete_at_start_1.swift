#^A^#

// Make sure we can code complete at the beginning of the file.
// rdar://14585108
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=A | FileCheck %s -check-prefix=A
// A: Begin completions
// A-DAG: Keyword/None: true[#Bool#]{{; name=.+$}}
// A-DAG: Keyword/None: false[#Bool#]{{; name=.+$}}
// A-DAG: Keyword/None: nil{{; name=.+$}}
// A-DAG: Decl[Struct]/OtherModule: Int8[#Int8#]{{; name=.+$}}
// A-DAG: Decl[Struct]/OtherModule: Int16[#Int16#]{{; name=.+$}}
// A-DAG: Decl[Struct]/OtherModule: Int32[#Int32#]{{; name=.+$}}
// A-DAG: Decl[Struct]/OtherModule: Int64[#Int64#]{{; name=.+$}}
// A-DAG: Decl[Struct]/OtherModule: Bool[#Bool#]{{; name=.+$}}
// A: End completions

// This function just adds more non-comment tokens to ensure that the file is not empty.
func foo() {}

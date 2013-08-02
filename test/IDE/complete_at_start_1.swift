#^A^#

// Make sure we can code complete at the beginning of the file.
// rdar://14585108
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=A | FileCheck %s -check-prefix=A
// A: Begin completions
// A-DAG: SwiftDecl: true[#Bool#]{{$}}
// A-DAG: SwiftDecl: false[#Bool#]{{$}}
// A-DAG: SwiftDecl: Int8[#Int8.metatype#]{{$}}
// A-DAG: SwiftDecl: Int16[#Int16.metatype#]{{$}}
// A-DAG: SwiftDecl: Int32[#Int32.metatype#]{{$}}
// A-DAG: SwiftDecl: Int64[#Int64.metatype#]{{$}}
// A-DAG: SwiftDecl: Int128[#Int128.metatype#]{{$}}
// A-DAG: SwiftDecl: Bool[#Bool.metatype#]{{$}}
// A: End completions

// This function just adds more non-comment tokens to ensure that the file is not empty.
func foo() {}

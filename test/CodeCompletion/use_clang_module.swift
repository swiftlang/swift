// RUN: %swift-ide-test -code-completion -source-filename %s -I %S/Inputs/custom-modules -code-completion-token=T1 | FileCheck %s -check-prefix=T1

import FooClangModule

func test() {
  #^T1^#
// T1: Begin completions
// T1-DAG: ClangDecl: FooEnum1{{$}}
// T1-DAG: ClangDecl: FooEnum1X{{$}}
// T1-DAG: ClangDecl: FooEnum2{{$}}
// T1-DAG: ClangDecl: FooEnum2Y{{$}}
// T1-DAG: ClangDecl: FooEnum2X{{$}}
// T1-DAG: ClangDecl: FooEnum3{{$}}
// T1-DAG: ClangDecl: FooEnum3Y{{$}}
// T1-DAG: ClangDecl: FooEnum3X{{$}}
// T1-DAG: ClangDecl: FooStruct1{{$}}
// T1-DAG: ClangDecl: FooStruct2{{$}}
// T1-DAG: ClangDecl: FooStructTypedef1{{$}}
// T1-DAG: ClangDecl: FooStructTypedef2{{$}}
// T1-DAG: ClangDecl: FooTypedef1{{$}}
// T1-DAG: ClangDecl: fooIntVar{{$}}
// T1-DAG: ClangDecl: fooFunc1{{$}}
// T1: End completions
}


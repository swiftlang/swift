// RUN: %swift-ide-test -code-completion -source-filename %s -F %S/Inputs/mock-sdk -code-completion-token=FW_UNQUAL_1 | FileCheck %s -check-prefix=FW_UNQUAL_1
// RUN: %swift-ide-test -code-completion -source-filename %s -F %S/Inputs/mock-sdk -code-completion-token=FW_QUAL_1 | FileCheck %s -check-prefix=FW_QUAL_1
// RUN: %swift-ide-test -code-completion -source-filename %s -F %S/Inputs/mock-sdk -code-completion-token=FW_QUAL_2 | FileCheck %s -check-prefix=FW_QUAL_2

import Foo
// Framework 'Foo' re-exports the 'FooHelper' framework.  Make sure that we get
// completion results for both frameworks.

func testClangModule() {
  #^FW_UNQUAL_1^#
// FW_UNQUAL_1: Begin completions
// FW_UNQUAL_1-DAG: SwiftDecl: fooFrameworkFunc1({#a: CInt#})[#CInt#]{{$}}
// FW_UNQUAL_1-DAG: SwiftDecl: fooHelperFrameworkFunc1({#a: CInt#})[#CInt#]{{$}}
// FW_UNQUAL_1: End completions
}

func testCompleteModuleQualifiedFoo1() {
  Foo.#^FW_QUAL_1^#
// FW_QUAL_1: Begin completions
// FW_QUAL_1-DAG: SwiftDecl: fooFrameworkFunc1({#a: CInt#})[#CInt#]{{$}}
// FW_QUAL_1-DAG: SwiftDecl: fooHelperFrameworkFunc1({#a: CInt#})[#CInt#]{{$}}
// FW_QUAL_1: End completions
}

func testCompleteModuleQualifiedFoo2() {
  Foo#^FW_QUAL_2^#
// FW_QUAL_2: Begin completions
// FW_QUAL_2-DAG: SwiftDecl: .fooFrameworkFunc1({#a: CInt#})[#CInt#]{{$}}
// FW_QUAL_2-DAG: SwiftDecl: .fooHelperFrameworkFunc1({#a: CInt#})[#CInt#]{{$}}
// FW_QUAL_2: End completions
}


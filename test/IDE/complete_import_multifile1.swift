// RUN: %target-swift-ide-test -code-completion -source-filename %s -F %S/Inputs/mock-sdk -code-completion-token=CLANG_IMPORT1 | %FileCheck %s -check-prefix=CLANG_IMPORT1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -second-source-filename %S/Inputs/complete_import_multifile2.swift -F %S/Inputs/mock-sdk -code-completion-token=CLANG_IMPORT1 | %FileCheck %s -check-prefix=CLANG_IMPORT1
// REQUIRES: objc_interop

import #^CLANG_IMPORT1^#

// CLANG_IMPORT1:	Begin completions
// CLANG_IMPORT1-DAG:	Decl[Module]/OtherModule[Foo]:                       Foo[#Module#]; name=Foo
// CLANG_IMPORT1-DAG:	Decl[Module]/OtherModule[FooHelper]:                 FooHelper[#Module#]; name=FooHelper
// CLANG_IMPORT1-DAG:	Decl[Module]/OtherModule[Bar]:                       Bar[#Module#]; name=Bar
// CLANG_IMPORT1-NOT:	SwiftShims

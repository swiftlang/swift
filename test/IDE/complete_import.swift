// RUN: %target-swift-ide-test -code-completion -source-filename %s -F %S/Inputs/mock-sdk -code-completion-token=CLANG_IMPORT1 | FileCheck %s -check-prefix=CLANG_IMPORT1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -F %S/Inputs/mock-sdk -code-completion-token=CLANG_IMPORT2 | FileCheck %s -check-prefix=CLANG_IMPORT1

import #^CLANG_IMPORT1^#

// CLANG_IMPORT1: Begin completions
// CLANG_IMPORT1-DAG: 	Keyword/None:                       Foo[#Module#]; name=Foo
// CLANG_IMPORT1-DAG:	Keyword/None:                       FooHelper[#Module#]; name=FooHelper
// CLANG_IMPORT1-DAG: 	Keyword/None:                       Bar[#Module#]; name=Bar

import Foo

import #^CLANG_IMPORT2^#

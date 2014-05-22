import Foundation

// RUN: rm -rf %t/clang-module-cache
// RUN: %swift-ide-test -sdk=%sdk -module-cache-path %t/clang-module-cache -code-completion -source-filename %s -code-completion-token=PLAIN_TOP_LEVEL_1 > %t.toplevel.txt
// RUN: FileCheck %s -check-prefix=PLAIN_TOP_LEVEL < %t.toplevel.txt
// RUN: FileCheck %s -check-prefix=NO_STDLIB_PRIVATE < %t.toplevel.txt

// RUN: %swift-ide-test -sdk=%sdk -module-cache-path %t/clang-module-cache -code-completion -source-filename %s -code-completion-token=PRIVATE_NOMINAL_MEMBERS_1 > %t.members.txt
// RUN: FileCheck %s -check-prefix=PRIVATE_NOMINAL_MEMBERS_1 < %t.members.txt
// RUN: FileCheck %s -check-prefix=NO_STDLIB_PRIVATE < %t.members.txt

// <rdar://problem/16996336> %swift-ide-test should include -resource-dir
// REQUIRES: OS=macosx

// NO_STDLIB_PRIVATE: Begin completions
// NO_STDLIB_PRIVATE-NOT: _convertStringToNSString
// NO_STDLIB_PRIVATE: End completions

#^PLAIN_TOP_LEVEL_1^#

// PLAIN_TOP_LEVEL: Begin completions
// PLAIN_TOP_LEVEL-DAG: Decl[GlobalVar]/OtherModule: true[#Bool#]{{$}}
// PLAIN_TOP_LEVEL: End completions

func privateNominalMembers(a: String) {
  a.#^PRIVATE_NOMINAL_MEMBERS_1^#
}

// PRIVATE_NOMINAL_MEMBERS_1: Begin completions
// PRIVATE_NOMINAL_MEMBERS_1-DAG: Decl[InstanceVar]/CurrNominal: startIndex[#String.Index#]{{$}}
// PRIVATE_NOMINAL_MEMBERS_1: End completions


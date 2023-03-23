import Foundation

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PLAIN_TOP_LEVEL_1 > %t.toplevel.txt
// RUN: %FileCheck %s -check-prefix=PLAIN_TOP_LEVEL < %t.toplevel.txt
// RUN: %FileCheck %s -check-prefix=NO_STDLIB_PRIVATE < %t.toplevel.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PRIVATE_NOMINAL_MEMBERS_1 > %t.members.txt
// RUN: %FileCheck %s -check-prefix=PRIVATE_NOMINAL_MEMBERS_1 < %t.members.txt
// RUN: %FileCheck %s -check-prefix=NO_STDLIB_PRIVATE < %t.members.txt

// REQUIRES: objc_interop

// NO_STDLIB_PRIVATE-NOT: _convertErrorToNSError

#^PLAIN_TOP_LEVEL_1^#

// PLAIN_TOP_LEVEL-DAG: Decl[Struct]/OtherModule[Foundation.NSURL]/IsSystem: URLResourceKey[#URLResourceKey#]{{; name=.+}}

func privateNominalMembers(a: String) {
  a.#^PRIVATE_NOMINAL_MEMBERS_1^#
}


// FIXME: we should show the qualified String.Index type.
// rdar://problem/20788802
// PRIVATE_NOMINAL_MEMBERS_1-DAG: Decl[InstanceVar]/CurrNominal/IsSystem: startIndex[#String.Index#]{{; name=.+$}}

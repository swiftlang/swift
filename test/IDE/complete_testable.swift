// RUN: rm -rf %t && mkdir %t
// RUN: %target-swift-frontend -emit-module -enable-testing -o %t %S/Inputs/complete_testable_helper.swift

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL -I %t > %t.txt
// RUN: FileCheck %s -check-prefix=TOP_LEVEL -check-prefix=TOP_LEVEL-ALL < %t.txt
// RUN: FileCheck %s -check-prefix=TOP_LEVEL-NEG -check-prefix=TOP_LEVEL-ALL-NEG < %t.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL -I %t -D TESTABLE > %t.testable.txt
// RUN: FileCheck %s -check-prefix=TOP_LEVEL-TESTABLE -check-prefix=TOP_LEVEL-ALL < %t.testable.txt
// RUN: FileCheck %s -check-prefix=TOP_LEVEL-TESTABLE-NEG -check-prefix=TOP_LEVEL-ALL-NEG < %t.testable.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER -I %t > %t.txt
// RUN: FileCheck %s -check-prefix=MEMBER -check-prefix=MEMBER-ALL < %t.txt
// RUN: FileCheck %s -check-prefix=MEMBER-NEG -check-prefix=MEMBER-ALL-NEG < %t.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER -I %t -D TESTABLE > %t.testable.txt
// RUN: FileCheck %s -check-prefix=MEMBER-TESTABLE -check-prefix=MEMBER-ALL < %t.testable.txt
// RUN: FileCheck %s -check-prefix=MEMBER-TESTABLE-NEG -check-prefix=MEMBER-ALL-NEG < %t.testable.txt

#if TESTABLE
@testable import complete_testable_helper
#else
import complete_testable_helper
#endif

#^TOP_LEVEL^#

// TOP_LEVEL-ALL: Begin completions
// TOP_LEVEL-ALL-DAG: Decl[Struct]/OtherModule[complete_testable_helper]: PublicStruct[#PublicStruct#]; name=PublicStruct
// FIXME-TESTABLE-DAG: Decl[Struct]/OtherModule[complete_testable_helper]: InternalStruct[#InternalStruct#]; name=InternalStruct
// TOP_LEVEL-ALL: End completions

// TOP_LEVEL-NEG-NOT: InternalStruct
// FIXME: TOP_LEVEL-TESTABLE-NEG-NOT: InternalStruct
// TOP_LEVEL-ALL-NEG-NOT: PrivateStruct

func test(value: PublicStruct) {
  value.#^MEMBER^#

  // MEMBER-ALL: Begin completions
  // MEMBER-ALL-DAG: Decl[InstanceMethod]/CurrNominal:   publicMethod()[#Void#]; name=publicMethod()
  // MEMBER-TESTABLE-DAG: Decl[InstanceMethod]/CurrNominal:   internalMethod()[#Void#]; name=internalMethod()
  // MEMBER-ALL: End completions

  // MEMBER-NEG-NOT: internalMethod
  // MEMBER-ALL-NEG-NOT: privateMethod
}

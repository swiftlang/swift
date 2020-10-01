// RUN: %empty-directory(%t)
// RUN: %target-swift-ide-test -code-completion -source-filename %s -enable-cross-import-overlays -I %S/Inputs/CrossImport -code-completion-token=COMPLETE > %t/results.tmp
// RUN: %FileCheck --input-file %t/results.tmp --check-prefix=COMPLETE %s
// RUN: %FileCheck --input-file %t/results.tmp --check-prefix=COMPLETE-NEGATIVE %s

// RUN: %target-swift-ide-test -code-completion -source-filename %s -enable-cross-import-overlays -I %S/Inputs/CrossImport -code-completion-token=IMPORT > %t/results.tmp
// RUN: %FileCheck --input-file %t/results.tmp --check-prefix=IMPORT %s
// RUN: %FileCheck --input-file %t/results.tmp --check-prefix=IMPORT-NEGATIVE %s

// RUN: %target-swift-ide-test -code-completion -source-filename %s -enable-cross-import-overlays -I %S/Inputs/CrossImport -code-completion-token=SCOPED > %t/results.tmp
// RUN: %FileCheck --input-file %t/results.tmp --check-prefix=SCOPED %s
// RUN: %FileCheck --input-file %t/results.tmp --check-prefix=SCOPED-NEGATIVE %s

import A
import B

func foo() {
  #^COMPLETE^#
}

// COMPLETE-DAG: Decl[Module]/None:  swift_ide_test[#Module#]; name=swift_ide_test
// COMPLETE-DAG: Decl[Module]/None/IsSystem:  Swift[#Module#]; name=Swift
// COMPLETE-DAG: Decl[Module]/None:  B[#Module#]; name=B
// COMPLETE-DAG: Decl[Module]/None:  A[#Module#]; name=A
// COMPLETE-DAG: Decl[FreeFunction]/OtherModule[B]:  fromB()[#Void#]; name=fromB()
// COMPLETE-DAG: Decl[FreeFunction]/OtherModule[A]:  from_ABAdditions()[#Void#]; name=from_ABAdditions()
// COMPLETE-DAG: Decl[FreeFunction]/OtherModule[A]:  fromA()[#Void#]; name=fromA()

// COMPLETE-NEGATIVE-NOT: [_ABAdditions]
// COMPLETE-NEGATIVE-NOT: [__ABAdditionsDAdditions]
// COMPLETE-NEGATIVE-NOT: name=_ABAdditions
// COMPLETE-NEGATIVE-NOT: name=__ABAdditionsDAdditions


import #^IMPORT^#

// IMPORT-DAG: Decl[Module]/None/NotRecommended:   A[#Module#]; name=A
// IMPORT-DAG: Decl[Module]/None/NotRecommended:   B[#Module#]; name=B
// IMPORT-DAG: Decl[Module]/None:                  C[#Module#]; name=C
// IMPORT-DAG: Decl[Module]/None:                  D[#Module#]; name=D
// IMPORT-DAG: Decl[Module]/None:                  DBAdditions[#Module#]; name=DBAdditions

// IMPORT-NEGATIVE-NOT: _ABAdditions
// IMPORT-NEGATIVE-NOT: __ABAdditionsDAdditions


func bar() {
	A.#^SCOPED^#
}

// SCOPED-DAG: Decl[FreeFunction]/OtherModule[A]:  from_ABAdditions()[#Void#]; name=from_ABAdditions()
// SCOPED-DAG: Decl[FreeFunction]/OtherModule[A]:  fromA()[#Void#]; name=fromA()

// SCOPED-NEGATIVE-NOT: name=_ABAdditions
// SCOPED-NEGATIVE-NOT: name=__ABAdditionsDAdditions
// SCOPED-NEGATIVE-NOT: name=fromB
// SCOPED-NEGATIVE-NOT: [_ABAdditions]
// SCOPED-NEGATIVE-NOT: [__ABAdditionsDAdditions]


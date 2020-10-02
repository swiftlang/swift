// RUN: %empty-directory(%t)
// RUN: %target-swift-ide-test -code-completion -source-filename %s -enable-cross-import-overlays -I %S/Inputs/CrossImport -code-completion-token=COMPLETE > %t/results.tmp
// RUN: %FileCheck --input-file %t/results.tmp --check-prefix=COMPLETE %s
// RUN: %FileCheck --input-file %t/results.tmp --check-prefix=COMPLETE-NEGATIVE %s

// RUN: %target-swift-ide-test -code-completion -source-filename %s -enable-cross-import-overlays -I %S/Inputs/CrossImport -code-completion-token=IMPORT > %t/results.tmp
// RUN: %FileCheck --input-file %t/results.tmp --check-prefix=IMPORT %s
// RUN: %FileCheck --input-file %t/results.tmp --check-prefix=IMPORT-NEGATIVE %s

// D has a cross import with B, DBAdditions, that isn't underscored.
import D
import B

func foo() {
  #^COMPLETE^#
}

// COMPLETE-DAG: Decl[Module]/None:  swift_ide_test[#Module#]; name=swift_ide_test
// COMPLETE-DAG: Decl[Module]/None/IsSystem:  Swift[#Module#]; name=Swift
// COMPLETE-DAG: Decl[Module]/None:  B[#Module#]; name=B
// COMPLETE-DAG: Decl[Module]/None:  D[#Module#]; name=D
// COMPLETE-DAG: Decl[FreeFunction]/OtherModule[B]:  fromB()[#Void#]; name=fromB()
// COMPLETE-DAG: Decl[FreeFunction]/OtherModule[DBAdditions]:  fromDBAdditions()[#Void#]; name=fromDBAdditions()
// COMPLETE-DAG: Decl[FreeFunction]/OtherModule[D]:  fromD()[#Void#]; name=fromD()

// COMPLETE-NEGATIVE-NOT: [_ABAdditions]
// COMPLETE-NEGATIVE-NOT: [__ABAdditionsDAdditions]
// COMPLETE-NEGATIVE-NOT: name=_ABAdditions
// COMPLETE-NEGATIVE-NOT: name=__ABAdditionsDAdditions


import #^IMPORT^#

// IMPORT-DAG: Decl[Module]/None:                  A[#Module#]; name=A
// IMPORT-DAG: Decl[Module]/None/NotRecommended:   B[#Module#]; name=B
// IMPORT-DAG: Decl[Module]/None:                  C[#Module#]; name=C
// IMPORT-DAG: Decl[Module]/None/NotRecommended:   D[#Module#]; name=D
// IMPORT-DAG: Decl[Module]/None/NotRecommended:   DBAdditions[#Module#]; name=DBAdditions

// IMPORT-NEGATIVE-NOT: _ABAdditions
// IMPORT-NEGATIVE-NOT: __ABAdditionsDAdditions

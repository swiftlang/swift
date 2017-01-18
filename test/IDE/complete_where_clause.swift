// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GP1 | %FileCheck %s -check-prefix=A1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GP2 | %FileCheck %s -check-prefix=A1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GP3 | %FileCheck %s -check-prefix=A1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GP4 | %FileCheck %s -check-prefix=TYPE1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GP5 | %FileCheck %s -check-prefix=TYPE1

class A1<T1, T2, T3> {}

class A2<T4, T5> {}

protocol P1 {}

extension A1 where #^GP1^#{}

extension A1 where T1 : P1, #^GP2^# {}

extension A1 where T1 : P1, #^GP3^#

extension A1 where T1 : #^GP4^#

extension A1 where T1 : P1, T2 : #^GP5^#

// A1: Begin completions
// A1-DAG: Decl[GenericTypeParam]/Local:       T1[#T1#]; name=T1
// A1-DAG: Decl[GenericTypeParam]/Local:       T2[#T2#]; name=T2
// A1-DAG: Decl[GenericTypeParam]/Local:       T3[#T3#]; name=T3
// A1-NOT: T4
// A1-NOT: T5

// TYPE1: Begin completions
// TYPE1-DAG: Decl[Protocol]/CurrModule:          P1[#P1#]; name=P1
// TYPE1-DAG: Decl[Class]/CurrModule:             A1[#A1#]; name=A1
// TYPE1-DAG: Decl[Class]/CurrModule:             A2[#A2#]; name=A2
// TYPE1-NOT: T1
// TYPE1-NOT: T2
// TYPE1-NOT: T3
// TYPE1-NOT: T4
// TYPE1-NOT: T5

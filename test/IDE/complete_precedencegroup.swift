// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ATTRIBUTE_LIST_1 | %FileCheck %s -check-prefix=ATTRIBUTE_LIST
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ATTRIBUTE_LIST_2 | %FileCheck %s -check-prefix=ATTRIBUTE_LIST
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ATTRIBUTE_LIST_3 | %FileCheck %s -check-prefix=ATTRIBUTE_LIST
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ATTRIBUTE_LIST_4 | %FileCheck %s -check-prefix=ATTRIBUTE_LIST

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PRECEDENCE_GROUP_1 | %FileCheck %s -check-prefix=PRECEDENCE_GROUP
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PRECEDENCE_GROUP_2 | %FileCheck %s -check-prefix=PRECEDENCE_GROUP
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PRECEDENCE_GROUP_3 | %FileCheck %s -check-prefix=PRECEDENCE_GROUP
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PRECEDENCE_GROUP_4 | %FileCheck %s -check-prefix=PRECEDENCE_GROUP
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PRECEDENCE_GROUP_5 | %FileCheck %s -check-prefix=PRECEDENCE_GROUP

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PRECEDENCE_GROUP_1 %S/Inputs/precedencegroup_multifile.swift | %FileCheck %s -check-prefix=PRECEDENCE_GROUP_MULTIFILE

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PRECEDENCE_GROUP_CURRFILE | %FileCheck %s -check-prefix=PRECEDENCE_GROUP_CURRFILE

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ASSOCIATIVITY_1 | %FileCheck %s -check-prefix=ASSOCIATIVITY
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ASSOCIATIVITY_2 | %FileCheck %s -check-prefix=ASSOCIATIVITY

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ASSIGNMENT_1 | %FileCheck %s -check-prefix=ASSIGNMENT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ASSIGNMENT_2 | %FileCheck %s -check-prefix=ASSIGNMENT

infix operator +++: #^PRECEDENCE_GROUP_1^#

precedencegroup MyPrecedence1 {
  #^ATTRIBUTE_LIST_1^#
}
precedencegroup MyPrecedence2 {
  associativity: left #^ATTRIBUTE_LIST_2^#
}
precedencegroup MyPrecedence3 {
  higherThan: AdditionPrecedence
  lowerThan: MultiplicationPrecedence
  #^ATTRIBUTE_LIST_3^#
}
precedencegroup MyPrecedence4 {
  assignment: true
  #^ATTRIBUTE_LIST_4^#
  higherThan: AdditionPrecedence
  associativity: right
  lowerThan: MultiplicationPrecedence
}

precedencegroup MyPrecedence5 {
  associativity: #^ASSOCIATIVITY_1^#
  assignment: false
}
precedencegroup MyPrecedence6 {
  associativity: left
  lowerThan: #^PRECEDENCE_GROUP_2^#
  higherThan: AdditionPrecedence
}
precedencegroup MyPrecedence7 {
  associativity: none
  assignment: false
  lowerThan: AdditionPrecedence
  higherThan: #^PRECEDENCE_GROUP_3^#
}
precedencegroup MyPrecedence8 {
  associativity: right
  assignment: #^ASSIGNMENT_1^#
}
precedencegroup MyPrecedence9 {
  higherThan: AdditionPrecedence, #^PRECEDENCE_GROUP_4^#
}

// Test that we have completions despite successive attributes being invalid
precedencegroup MyPrecedence10 {
  associativity: #^ASSOCIATIVITY_2^#
  assignment: 5
  higherTh:
}
precedencegroup MyPrecedence11 {
  assignment: #^ASSIGNMENT_2^#
  lower:
  associativity: true
}
precedencegroup MyPrecedence12 {
  higherThan: #^PRECEDENCE_GROUP_5^#
  associativity: 3
  lowerTh:
}

infix operator ---: #^PRECEDENCE_GROUP_CURRFILE^#

// ATTRIBUTE_LIST: Begin completions, 4 items
// ATTRIBUTE_LIST: Keyword/None: associativity; name=associativity
// ATTRIBUTE_LIST: Keyword/None: higherThan;    name=higherThan
// ATTRIBUTE_LIST: Keyword/None: lowerThan;     name=lowerThan
// ATTRIBUTE_LIST: Keyword/None: assignment;    name=assignment

// ASSOCIATIVITY: Begin completions, 3 items
// ASSOCIATIVITY: Keyword/None: none;   name=none
// ASSOCIATIVITY: Keyword/None: left;   name=left
// ASSOCIATIVITY: Keyword/None: right;  name=right

// ASSIGNMENT: Begin completions, 2 items
// ASSIGNMENT: Keyword[false]/None: false; name=false
// ASSIGNMENT: Keyword[true]/None: true;   name=true

// PRECEDENCE_GROUP: Begin completions
// PRECEDENCE_GROUP-DAG: Decl[PrecedenceGroup]/OtherModule[Swift]: AssignmentPrecedence; name=AssignmentPrecedence
// PRECEDENCE_GROUP-DAG: Decl[PrecedenceGroup]/OtherModule[Swift]: ComparisonPrecedence; name=ComparisonPrecedence

/* FIXME: SR-8898 We only see precedence groups that are earlier in life! */
// PRECEDENCE_GROUP_CURRFILE-DAG: Begin completions
// PRECEDENCE_GROUP_CURRFILE-DAG: Decl[PrecedenceGroup]/CurrModule: MyPrecedence{{[0-9]+}};
// PRECEDENCE_GROUP_CURRFILE-DAG: Decl[PrecedenceGroup]/CurrModule: MyPrecedence{{[0-9]+}};
// PRECEDENCE_GROUP_CURRFILE-DAG: Decl[PrecedenceGroup]/CurrModule: MyPrecedence{{[0-9]+}};
// PRECEDENCE_GROUP_CURRFILE-DAG: Decl[PrecedenceGroup]/CurrModule: MyPrecedence{{[0-9]+}};
// PRECEDENCE_GROUP_CURRFILE-DAG: Decl[PrecedenceGroup]/CurrModule: MyPrecedence{{[0-9]+}};
// PRECEDENCE_GROUP_CURRFILE-DAG: Decl[PrecedenceGroup]/CurrModule: MyPrecedence{{[0-9]+}};
// PRECEDENCE_GROUP_CURRFILE-DAG: Decl[PrecedenceGroup]/CurrModule: MyPrecedence{{[0-9]+}};
// PRECEDENCE_GROUP_CURRFILE-DAG: Decl[PrecedenceGroup]/CurrModule: MyPrecedence{{[0-9]+}};
// PRECEDENCE_GROUP_CURRFILE-DAG: Decl[PrecedenceGroup]/CurrModule: MyPrecedence{{[0-9]+}};
// PRECEDENCE_GROUP_CURRFILE-DAG: Decl[PrecedenceGroup]/CurrModule: MyPrecedence{{[0-9]+}};
// PRECEDENCE_GROUP_CURRFILE-DAG: Decl[PrecedenceGroup]/CurrModule: MyPrecedence{{[0-9]+}};
// PRECEDENCE_GROUP_CURRFILE-DAG: Decl[PrecedenceGroup]/CurrModule: MyPrecedence{{[0-9]+}};

// PRECEDENCE_GROUP_MULTIFILE: Decl[PrecedenceGroup]/CurrModule: PrecedenceGroupOtherFile; name=PrecedenceGroupOtherFile

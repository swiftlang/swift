// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=POUND_NOMINAL_TOP | %FileCheck %s -check-prefix=POUND_DIRECTIVE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=POUND_NOMINAL_IN_IF | %FileCheck %s -check-prefix=POUND_DIRECTIVE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=POUND_NOMINAL_IN_ELSEIF | %FileCheck %s -check-prefix=POUND_DIRECTIVE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=POUND_NOMINAL_IN_ELSE | %FileCheck %s -check-prefix=POUND_DIRECTIVE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=POUND_GLOBAL_TOP | %FileCheck %s -check-prefix=POUND_DIRECTIVE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=POUND_GLOBAL_IN_IF | %FileCheck %s -check-prefix=POUND_DIRECTIVE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=POUND_GLOBAL_IN_ELSEIF | %FileCheck %s -check-prefix=POUND_DIRECTIVE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=POUND_GLOBAL_IN_ELSE | %FileCheck %s -check-prefix=POUND_DIRECTIVE

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONDITION_NOMINAL_1 | %FileCheck %s -check-prefix=CONDITION -check-prefix=NOFLAG
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONDITION_NOMINAL_1 -D FOO -D BAR | %FileCheck %s -check-prefix=CONDITION -check-prefix=WITHFLAG
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONDITION_NOMINAL_2 | %FileCheck %s -check-prefix=CONDITION -check-prefix=NOFLAGlll
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONDITION_GLOBAL_1 | %FileCheck %s -check-prefix=CONDITION -check-prefix=NOFLAG
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONDITION_GLOBAL_2 | %FileCheck %s -check-prefix=CONDITION -check-prefix=NOFLAG
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONDITION_GLOBAL_2 -D FOO -D BAR | %FileCheck %s -check-prefix=CONDITION -check-prefix=WITHFLAG

// POUND_DIRECTIVE: Begin completions, 7 items
// POUND_DIRECTIVE-DAG: Keyword[#sourceLocation]/None:      sourceLocation(file: {#String#}, line: {#Int#}); name=sourceLocation(file: String, line: Int)
// POUND_DIRECTIVE-DAG: Keyword[#warning]/None:             warning("{#(message)#}"); name=warning("message")
// POUND_DIRECTIVE-DAG: Keyword[#error]/None:               error("{#(message)#}"); name=error("message")
// POUND_DIRECTIVE-DAG: Keyword[#if]/None:                  if {#(condition)#}; name=if condition
// POUND_DIRECTIVE-DAG: Keyword[#elseif]/None:              elseif {#(condition)#}; name=elseif condition
// POUND_DIRECTIVE-DAG: Keyword[#else]/None:                else; name=else
// POUND_DIRECTIVE-DAG: Keyword[#endif]/None:               endif; name=endif

class C {
##^POUND_NOMINAL_TOP^#

#if true
  ##^POUND_NOMINAL_IN_IF^#
#elseif true
  ##^POUND_NOMINAL_IN_ELSEIF^#
#else
  ##^POUND_NOMINAL_IN_ELSE^#
#endif
}

##^POUND_GLOBAL_TOP^#

#if false
  ##^POUND_GLOBAL_IN_IF^#
#elseif false
  ##^POUND_GLOBAL_IN_ELSEIF^#
#else
  ##^POUND_GLOBAL_IN_ELSE^#
#endif

// CONDITION: Begin completions
// CONDITION-NOT: globalVar
// CONDITION-DAG: Pattern/ExprSpecific:               os({#(name)#}); name=os(name)
// CONDITION-DAG: Pattern/ExprSpecific:               arch({#(name)#}); name=arch(name)
// CONDITION-DAG: Pattern/ExprSpecific:               canImport({#(module)#}); name=canImport(module)
// CONDITION-DAG: Pattern/ExprSpecific:               targetEnvironment(simulator); name=targetEnvironment(simulator)
// CONDITION-DAG: Pattern/ExprSpecific:               swift(>={#(version)#}); name=swift(>=version)
// CONDITION-DAG: Pattern/ExprSpecific:               swift(<{#(version)#}); name=swift(<version)
// CONDITION-DAG: Pattern/ExprSpecific:               compiler(>={#(version)#}); name=compiler(>=version)
// CONDITION-DAG: Pattern/ExprSpecific:               compiler(<{#(version)#}); name=compiler(<version)
// CONDITION-DAG: Keyword[true]/None:                 true[#Bool#]; name=true
// CONDITION-DAG: Keyword[false]/None:                false[#Bool#]; name=false
// CONDITION-NOT: globalVar

// WITHFLAG: Keyword/ExprSpecific:               FOO; name=FOO
// WITHFLAG: Keyword/ExprSpecific:               BAR; name=BAR

// NOFLAG-NOT: FOO 
// NOFLAG-NOT: BAR

var globalVar = 1
extension C {
#if #^CONDITION_NOMINAL_1^#
#endif

#if true
#elseif false || (#^CONDITION_NOMINAL_2^#)
#endif
}

#if swift(>=1000) && #^CONDITION_GLOBAL_1^#
#endif

#if false
#elseif #^CONDITION_GLOBAL_2^#
// This '#if' is intentionally not closed with '#endif'

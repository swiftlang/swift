// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=POUND_NOMINAL_TOP | %FileCheck %s -check-prefix=POUND_DIRECTIVE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=POUND_NOMINAL_IN_IF | %FileCheck %s -check-prefix=POUND_DIRECTIVE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=POUND_NOMINAL_IN_ELSEIF | %FileCheck %s -check-prefix=POUND_DIRECTIVE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=POUND_NOMINAL_IN_ELSE | %FileCheck %s -check-prefix=POUND_DIRECTIVE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=POUND_GLOBAL_TOP | %FileCheck %s -check-prefix=POUND_DIRECTIVE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=POUND_GLOBAL_IN_IF | %FileCheck %s -check-prefix=POUND_DIRECTIVE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=POUND_GLOBAL_IN_ELSEIF | %FileCheck %s -check-prefix=POUND_DIRECTIVE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=POUND_GLOBAL_IN_ELSE | %FileCheck %s -check-prefix=POUND_DIRECTIVE

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


// RUN: %empty-directory(%t)
// RUN: %target-swift-ide-test -batch-code-completion -source-filename %s -filecheck %raw-FileCheck -completion-output-dir %t -parse-as-library

#^TOPLEVEL^#

// TOPLEVEL: Begin completions
// TOPLEVEL-DAG: Keyword[associatedtype]/None:       associatedtype; name=associatedtype
// TOPLEVEL-DAG: Keyword[class]/None/Flair[CommonKeyword]: class; name=class
// TOPLEVEL-DAG: Keyword[deinit]/None:               deinit; name=deinit
// TOPLEVEL-DAG: Keyword[enum]/None/Flair[CommonKeyword]: enum; name=enum
// TOPLEVEL-DAG: Keyword[extension]/None/Flair[CommonKeyword]: extension; name=extension
// TOPLEVEL-DAG: Keyword[func]/None:                 func; name=func
// TOPLEVEL-DAG: Keyword[import]/None:               import; name=import
// TOPLEVEL-DAG: Keyword[init]/None:                 init; name=init
// TOPLEVEL-DAG: Keyword[inout]/None:                inout; name=inout
// TOPLEVEL-DAG: Keyword[operator]/None:             operator; name=operator
// TOPLEVEL-DAG: Keyword[precedencegroup]/None:      precedencegroup; name=precedencegroup
// TOPLEVEL-DAG: Keyword[protocol]/None/Flair[CommonKeyword]: protocol; name=protocol
// TOPLEVEL-DAG: Keyword[struct]/None/Flair[CommonKeyword]: struct; name=struct
// TOPLEVEL-DAG: Keyword[subscript]/None:            subscript; name=subscript
// TOPLEVEL-DAG: Keyword[typealias]/None:            typealias; name=typealias
// TOPLEVEL-DAG: Keyword[fileprivate]/None:          fileprivate; name=fileprivate
// TOPLEVEL-DAG: Keyword[internal]/None:             internal; name=internal
// TOPLEVEL-DAG: Keyword[private]/None:              private; name=private
// TOPLEVEL-DAG: Keyword[public]/None:               public; name=public
// TOPLEVEL-DAG: Keyword[static]/None:               static; name=static
// TOPLEVEL-DAG: Keyword/None:                       final; name=final
// TOPLEVEL-DAG: Keyword/None:                       required; name=required
// TOPLEVEL-DAG: Keyword/None:                       optional; name=optional
// TOPLEVEL-DAG: Keyword/None:                       lazy; name=lazy
// TOPLEVEL-DAG: Keyword/None:                       dynamic; name=dynamic
// TOPLEVEL-DAG: Keyword/None:                       infix; name=infix
// TOPLEVEL-DAG: Keyword/None:                       prefix; name=prefix
// TOPLEVEL-DAG: Keyword/None:                       postfix; name=postfix
// TOPLEVEL-DAG: Keyword/None:                       mutating; name=mutating
// TOPLEVEL-DAG: Keyword/None:                       nonmutating; name=nonmutating
// TOPLEVEL-DAG: Keyword/None:                       convenience; name=convenience
// TOPLEVEL-DAG: Keyword/None:                       override; name=override
// TOPLEVEL-DAG: Keyword/None:                       open; name=open
// TOPLEVEL-DAG: Keyword/None:                       weak; name=weak
// TOPLEVEL-DAG: Keyword/None:                       unowned; name=unowned
// TOPLEVEL-DAG: Keyword/None:                       indirect; name=indirect
// TOPLEVEL-DAG: Keyword/None:                       nonisolated; name=nonisolated
// TOPLEVEL-DAG: Keyword[defer]/None:                defer; name=defer
// TOPLEVEL-DAG: Keyword[if]/None:                   if; name=if
// TOPLEVEL-DAG: Keyword[guard]/None:                guard; name=guard
// TOPLEVEL-DAG: Keyword[do]/None:                   do; name=do
// TOPLEVEL-DAG: Keyword[repeat]/None:               repeat; name=repeat
// TOPLEVEL-DAG: Keyword[else]/None:                 else; name=else
// TOPLEVEL-DAG: Keyword[for]/None:                  for; name=for
// TOPLEVEL-DAG: Keyword[in]/None:                   in; name=in
// TOPLEVEL-DAG: Keyword[while]/None:                while; name=while
// TOPLEVEL-DAG: Keyword[break]/None:                break; name=break
// TOPLEVEL-DAG: Keyword[continue]/None:             continue; name=continue
// TOPLEVEL-DAG: Keyword[fallthrough]/None:          fallthrough; name=fallthrough
// TOPLEVEL-DAG: Keyword[switch]/None:               switch; name=switch
// TOPLEVEL: End completions

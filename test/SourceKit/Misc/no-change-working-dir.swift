// RUN: %empty-directory(%t)

// Make sure that specifying '-working-directory' does not actually cause the
// working directory of the process to be affected.

// RUN: %sourcekitd-test -req=open -print-raw-response %s -- %s -working-directory %t > /dev/null
// RUN: %sourcekitd-test \
// RUN:   -req=open -req-opts=syntactic_only=1 -print-raw-response %s -- %s -working-directory %t == \
// RUN:   -req=edit -offset=0 -length=1 -replace="//" %s == \
// RUN:   -req=semantic-tokens %s -- %s -working-directory %t

// RUN: %sourcekitd-test -req=collect-var-type %s -- %s -working-directory %t
// RUN: %sourcekitd-test -req=collect-type %s -- %s -working-directory %t
// RUN: %sourcekitd-test -req=refactoring.extract.expr -pos=1:1 -end-pos 1:1 %s -- %s -working-directory %t
// RUN: %sourcekitd-test -req=active-regions %s -- %s -working-directory %t
// RUN: %sourcekitd-test -req=related-idents -pos=1:1 %s -- %s -working-directory %t
// RUN: %sourcekitd-test -req=range -pos=1:1 -length 1 %s -- %s -working-directory %t
// RUN: %sourcekitd-test -req=translate -pos=1:1 -swift-name foo %s -- %s -working-directory %t
// RUN: %sourcekitd-test -req=interface-gen %s -- %s -working-directory %t

func foo(x: Int) {}
// RUN: %sourcekitd-test -req=find-local-rename-ranges -pos=%(line-1):10 %s -- %s -working-directory %t

// RUN: %sourcekitd-test -req=doc-info -module Swift -- -working-directory %t > /dev/null
// RUN: %sourcekitd-test -req=cursor -pos=1:1 %s -- %s -working-directory %t > /dev/null

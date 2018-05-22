// RUN: not %sourcekitd-test -req=track-compiles == -req=complete %s -offset=0 -- %s -no-such-arg | %FileCheck %s -check-prefix=ARG_PARSE_1
// ARG_PARSE_1: {
// ARG_PARSE_1:  key.notification: source.notification.compile-will-start
// ARG_PARSE_1:  key.compileid: [[CID1:".*"]]
// ARG_PARSE_1:  key.compilerargs-string: "{{.*}}.swift -no-such-arg"
// ARG_PARSE_1: }
// ARG_PARSE_1: {
// ARG_PARSE_1:   key.notification: source.notification.compile-did-finish
// ARG_PARSE_1:   key.diagnostics: [
// FIXME: we should pass through the error from parsing the arguments
// ARG_PARSE_1-NEXT: ]
// ARG_PARSE_1:   key.compileid: [[CID1]]
// ARG_PARSE_1: }
// ARG_PARSE_1-NOT: compile-will-start
// ARG_PARSE_1-NOT: compile-did-finish

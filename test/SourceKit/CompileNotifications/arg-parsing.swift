// RUN: not %sourcekitd-test -req=track-compiles == -req=complete %s -offset=0 -- %s -no-such-arg | %FileCheck %s -check-prefix=ARG_PARSE_1
// ARG_PARSE_1: {
// ARG_PARSE_1:  key.notification: source.notification.compile-will-start
// ARG_PARSE_1:  key.compileid: [[CID1:".*"]]
// ARG_PARSE_1:  key.compilerargs-string: "{{.*}}.swift -no-such-arg"
// ARG_PARSE_1: }
// ARG_PARSE_1: {
// ARG_PARSE_1:   key.notification: source.notification.compile-did-finish
// ARG_PARSE_1:   key.diagnostics: [
// ARG_PARSE_1:     {
// ARG_PARSE_1:       key.filepath: "<unknown>",
// ARG_PARSE_1:       key.severity: source.diagnostic.severity.error,
// ARG_PARSE_1:       key.description: "unknown argument: '-no-such-arg'"
// ARG_PARSE_1:     }
// ARG_PARSE_1:   ]
// ARG_PARSE_1:   key.compileid: [[CID1]]
// ARG_PARSE_1: }
// ARG_PARSE_1-NOT: compile-will-start
// ARG_PARSE_1-NOT: compile-did-finish

// RUN: %sourcekitd-test -req=track-compiles == -req=sema %s -- %s | %FileCheck %s -check-prefix=ARGS1
// ARGS1: {
// ARGS1:  key.notification: source.notification.compile-will-start
// ARGS1:  key.filepath: "[[PATH:.*]]"
// ARGS1:  key.compilerargs: [
// ARGS1-NEXT:    [[PATH]]
// ARGS1-NEXT:  ]

// RUN: %sourcekitd-test -req=track-compiles == -req=sema %s -- %s -j 1000 | %FileCheck %s -check-prefix=ARGS2
// ARGS2: {
// ARGS2:  key.notification: source.notification.compile-will-start
// ARGS2:  key.filepath: "[[PATH:.*]]"
// ARGS2:  key.compilerargs: [
// ARGS2-NEXT:    [[PATH]]
// ARGS2-NEXT:    "-j"
// ARGS2-NEXT:    "1000"
// ARGS2-NEXT:  ]

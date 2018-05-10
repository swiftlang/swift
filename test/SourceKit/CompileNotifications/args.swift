// RUN: %sourcekitd-test -req=track-compiles == -req=sema %s -- %s | %FileCheck %s -check-prefix=ARGS1
// ARGS1: {
// ARGS1:  key.notification: source.notification.compile-will-start
// ARGS1:  key.filepath: "[[PATH:.*]]"
// ARGS1:  key.compilerargs-string: "
// ARGS1-LINE:    [[PATH]]

// RUN: %sourcekitd-test -req=track-compiles == -req=sema %s -- %s -j 1000 | %FileCheck %s -check-prefix=ARGS2
// ARGS2: {
// ARGS2:  key.notification: source.notification.compile-will-start
// ARGS2:  key.filepath: "[[PATH:.*]]"
// ARGS2:  key.compilerargs-string: "
// ARGS2-LINE:    [[PATH]]
// ARGS2-LINE:    -j
// ARGS2-LINE:    1000

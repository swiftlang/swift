// RUN: %sourcekitd-test -req=track-compiles == -req=complete %s -offset=0 -- %s | %FileCheck %s -check-prefix=COMPILE_1 --enable-yaml-compatibility
// COMPILE_1: {
// COMPILE_1:  key.notification: source.notification.compile-will-start,
// COMPILE_1:  key.filepath: "{{.*}}SOURCE_DIR{{.*}}code-completion.swift",
// COMPILE_1:  key.compileid: [[CID1:".*"]]
// COMPILE_1: }
// COMPILE_1: {
// COMPILE_1:   key.notification: source.notification.compile-did-finish,
// COMPILE_1:   key.compileid: [[CID1]]
// COMPILE_1: }
// COMPILE_1-NOT: compile-will-start
// COMPILE_1-NOT: compile-did-finish
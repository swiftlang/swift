// RUN: %sourcekitd-test -req=track-compiles == -req=cursor %s -offset=0 -- %s | %FileCheck %s -check-prefix=COMPILE_1 --enable-yaml-compatibility
// COMPILE_1: <empty cursor info; internal diagnostic: "Unable to resolve cursor info.">
// COMPILE_1: {
// COMPILE_1:  key.notification: source.notification.compile-will-start,
// COMPILE_1:  key.filepath: "SOURCE_DIR{{.*}}cursor-info.swift",
// COMPILE_1:  key.compileid: [[CID1:".*"]]
// COMPILE_1: }
// COMPILE_1: {
// COMPILE_1:   key.notification: source.notification.compile-did-finish,
// COMPILE_1:   key.compileid: [[CID1]]
// COMPILE_1: }
// FIXME: Once all cursor info kinds are migrated to solver-based and we remove the fallback path to AST-based cursor info, we should only receive a single compile notification
// COMPILE_1: {
// COMPILE_1:  key.notification: source.notification.compile-will-start,
// COMPILE_1:  key.filepath: "SOURCE_DIR{{.*}}cursor-info.swift",
// COMPILE_1:  key.compileid: [[CID2:".*"]]
// COMPILE_1: }
// COMPILE_1: {
// COMPILE_1:   key.notification: source.notification.compile-did-finish,
// COMPILE_1:   key.compileid: [[CID2]]
// COMPILE_1: }
// COMPILE_1-NOT: compile-will-start
// COMPILE_1-NOT: compile-did-finish

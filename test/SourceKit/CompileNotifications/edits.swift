// RUN: %sourcekitd-test -req=track-compiles \
// RUN:     == -req=sema %s -- %s \
// RUN:     == -req=edit %s -replace="" | %FileCheck %s -check-prefix=COMPILE_1
// COMPILE_1: {
// COMPILE_1:  key.notification: source.notification.compile-will-start,
// COMPILE_1:  key.compileid: [[CID1:".*"]]
// COMPILE_1: }
// COMPILE_1: {
// COMPILE_1:   key.notification: source.notification.compile-did-finish,
// COMPILE_1:   key.compileid: [[CID1]]
// COMPILE_1: }
// COMPILE_1-NOT: compile-will-start
// COMPILE_1-NOT: compile-did-finish

// RUN: %sourcekitd-test -req=track-compiles \
// RUN:     == -req=sema %s -- %s \
// RUN:     == -req=edit %s -replace="asdf" \
// RUN:     == -req=edit %s -replace="//" | %FileCheck %s -check-prefix=COMPILE_3
// COMPILE_3: {
// COMPILE_3:  key.notification: source.notification.compile-will-start,
// COMPILE_3:  key.compileid: [[CID1:".*"]]
// COMPILE_3: }
// COMPILE_3: {
// COMPILE_3:   key.notification: source.notification.compile-did-finish,
// COMPILE_3:   key.compileid: [[CID1]]
// COMPILE_3: }
// COMPILE_3: {
// COMPILE_3:  key.notification: source.notification.compile-will-start,
// COMPILE_3:  key.compileid: [[CID2:".*"]]
// COMPILE_3: }
// COMPILE_3: {
// COMPILE_3:   key.notification: source.notification.compile-did-finish,
// COMPILE_3:   key.compileid: [[CID2]]
// COMPILE_3: }
// COMPILE_3: {
// COMPILE_3:  key.notification: source.notification.compile-will-start,
// COMPILE_3:  key.compileid: [[CID3:".*"]]
// COMPILE_3: }
// COMPILE_3: {
// COMPILE_3:   key.notification: source.notification.compile-did-finish,
// COMPILE_3:   key.compileid: [[CID3]]
// COMPILE_3: }
// COMPILE_3-NOT: compile-will-start
// COMPILE_3-NOT: compile-did-finish

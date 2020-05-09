// RUN: %sourcekitd-test -req=sema %s -- %s | %FileCheck %s -check-prefix=NONE
// RUN: %sourcekitd-test -req=track-compiles == -req=version | %FileCheck %s -check-prefix=NONE
// RUN: %sourcekitd-test -req=track-compiles -req-opts=value=0 \
// RUN:     == -req=sema %s -- %s | %FileCheck %s -check-prefix=NONE
// NONE-NOT: compile-will-start
// NONE-NOT: compile-did-finish


// RUN: %sourcekitd-test -req=track-compiles == -req=sema %s -- %s | %FileCheck %s -check-prefix=COMPILE_1 --enable-yaml-compatibility
// RUN: %sourcekitd-test -req=track-compiles == -req=track-compiles == -req=sema %s -- %s | %FileCheck %s -check-prefix=COMPILE_1 --enable-yaml-compatibility
// RUN: %sourcekitd-test -req=track-compiles \
// RUN:     == -req=sema %s -- %s \
// RUN:     == -req=track-compiles -req-opts=value=0 \
// RUN:     == -req=edit %s -replace="//" | %FileCheck %s -check-prefix=COMPILE_1 --enable-yaml-compatibility
// RUN: %sourcekitd-test -req=sema %s -- %s \
// RUN:     == -req=track-compiles -req-opts=value=1 \
// RUN:     == -req=edit %s -replace="//" | %FileCheck %s -check-prefix=COMPILE_1 --enable-yaml-compatibility
// COMPILE_1: {
// COMPILE_1:  key.notification: source.notification.compile-will-start,
// COMPILE_1:  key.filepath: "SOURCE_DIR{{.*}}enable-disable.swift",
// COMPILE_1:  key.compileid: [[CID1:".*"]]
// COMPILE_1: }
// COMPILE_1: {
// COMPILE_1:   key.notification: source.notification.compile-did-finish,
// COMPILE_1:   key.compileid: [[CID1]]
// COMPILE_1: }
// COMPILE_1-NOT: compile-will-start
// COMPILE_1-NOT: compile-did-finish

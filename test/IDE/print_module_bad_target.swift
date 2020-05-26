// RUN: not %swift-ide-test -source-filename %s -print-module -module-to-print Swift -target x86_64-unknown-solaris

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t -module-name Dummy -target x86_64-apple-macosx10.99 %s
// RUN: not %target-swift-ide-test -source-filename %s -print-module -module-to-print Dummy -I %t

// REQUIRES: OS=macosx

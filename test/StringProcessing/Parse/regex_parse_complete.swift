// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Make sure we consider the below source complete.
// RUN: %swift-ide-test -test-input-complete -enable-bare-slash-regex -source-filename %t/bare-slash.swift | %FileCheck %s -check-prefix=COMPLETE

// Bare slash is currently disabled by default.
// RUN: %swift-ide-test -test-input-complete -source-filename %t/bare-slash.swift | %FileCheck %s -check-prefix=INCOMPLETE

// RUN: %swift-ide-test -test-input-complete -source-filename %t/extended.swift | %FileCheck %s -check-prefix=COMPLETE

// INCOMPLETE: IS_INCOMPLETE
// COMPLETE: IS_COMPLETE

//--- bare-slash.swift
/\(/

//--- extended.swift
#/\(/#

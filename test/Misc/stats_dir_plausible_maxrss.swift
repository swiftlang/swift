// RUN: %empty-directory(%t)
// RUN: touch %t/main.swift
// RUN: %target-swiftc_driver -o %t/main -module-name main -stats-output-dir %t %t/main.swift
// RUN: %utils/process-stats-dir.py --set-csv-baseline %t/frontend.csv %t
// RUN: %FileCheck -input-file %t/frontend.csv %s

// This test checks that we're reporting some number that's "at least 10MB" and
// "not more than 999MB", because for a while we were incorrectly reporting KB
// as bytes on non-macOS, so claiming we took (say) "100KB". If we ever manage
// to get the swift frontend to use less than 10MB, celebrate! And change this
// test. Likewise (minus celebration) if compiling a function like the following
// ever takes more than 1GB.
//
// CHECK: {{"Driver.ChildrenMaxRSS"	[1-9][0-9]{7,8}$}}

public func foo() {
    print("hello")
}

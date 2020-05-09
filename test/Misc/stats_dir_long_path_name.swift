// REQUIRES: OS=macosx
// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/very-long-directory-name-that-contains-over-128-characters-which-is-not-enough-to-be-illegal-on-its-own-but-presents
// RUN: cp %s %t/very-long-directory-name-that-contains-over-128-characters-which-is-not-enough-to-be-illegal-on-its-own-but-presents/a-problem-when-combined-with-other-long-path-elements-and-filenames-to-exceed-256-characters-combined-because-yay-arbitrary-limits-amirite.swift
// RUN: touch %t/main.swift
// RUN: %target-swiftc_driver -o %t/main -module-name main -stats-output-dir %t %t/main.swift %t/very-long-directory-name-that-contains-over-128-characters-which-is-not-enough-to-be-illegal-on-its-own-but-presents/a-problem-when-combined-with-other-long-path-elements-and-filenames-to-exceed-256-characters-combined-because-yay-arbitrary-limits-amirite.swift
// RUN: %{python} %utils/process-stats-dir.py --set-csv-baseline %t/frontend.csv %t
// RUN: %FileCheck -input-file %t/frontend.csv %s

// CHECK: {{"AST.NumSourceLines"	[1-9][0-9]*$}}
// CHECK: {{"IRModule.NumIRFunctions"	[1-9][0-9]*$}}
// CHECK: {{"LLVM.NumLLVMBytesOutput"	[1-9][0-9]*$}}

public func foo() {
    print("hello")
}

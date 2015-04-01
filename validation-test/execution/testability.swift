// RUN: rm -rf %t && mkdir %t

// This file is intended to match the run lines in
// test/Interpreter/testability.swift, but with -O added to all compilations.
// The next block verifies that.

// RUN: grep RUN: %s | grep -v VERIFY > %t/VERIFY.txt
// RUN: grep RUN: %S/../../test/Interpreter/testability.swift | sed -e 's|%%S|%%S/../../test/Interpreter|g' -e 's|%%s|%%S/../../test/Interpreter/testability.swift|g' -e 's|%%target-build-swift|%%target-build-swift -O|' > %t/VERIFY-orig.txt
// RUN: diff %t/VERIFY.txt %t/VERIFY-orig.txt

// RUN: %target-build-swift -O -emit-library -c %S/../../test/Interpreter/Inputs/testability_helper.swift -enable-testing -force-single-frontend-invocation -o %t/testability_helper.o -emit-module

// RUN: %target-build-swift -O %S/../../test/Interpreter/testability.swift -I %t -Xlinker %t/testability_helper.o -o %t/main
// RUN: %target-run %t/main | FileCheck %S/../../test/Interpreter/testability.swift

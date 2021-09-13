// RUN: %empty-directory(%t)
// RUN: for test in %S/Inputs/*; do testname=$(basename $test); echo $testname; %target-build-swift -o %t/$testname $test; %target-codesign %t/$testname; %target-run %t/$testname | %FileCheck $test || exit 1; done

// REQUIRES: executable_test
// REQUIRES: shell

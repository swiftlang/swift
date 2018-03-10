// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out
// RUN: not --crash %t/a.out 2>&1 | PYTHONPATH=%lldb-python-path %utils/symbolicate-linux-fatal %t/a.out - | %utils/backtrace-check -u
// REQUIRES: executable_test
// REQUIRES: OS=linux-gnu
// REQUIRES: lldb
// REQUIRES: rdar38181372

// Backtraces are not emitted when optimizations are enabled. This test can not
// run when optimizations are enabled.
// REQUIRES: swift_test_mode_optimize_none

func funcB() {
    fatalError("linux-fatal-backtrace");
}

func funcA() {
    funcB();
}

print("bla")
funcA()

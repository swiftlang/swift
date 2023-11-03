// Check a handful of failures in the driver when TMPDIR can't be accessed.
//
// These particular error messages are kind of finicky to hit because they
// depend on what commands the driver needs to execute on each platform, so the
// test is somewhat artificially limited to macOS.
//
// REQUIRES: OS=macosx
// REQUIRES: rdar117411265
// (https://github.com/apple/swift/issues/54796) This test is failing on
// next branch.
// XFAIL: *

// RUN: env TMP="%t/fake/" TMPDIR="%t/fake/" not %target-build-swift -c -driver-filelist-threshold=0 %s 2>&1 | %FileCheck -check-prefix=CHECK-SOURCES %s

// CHECK-SOURCES: - unable to create list of input sources

// RUN: echo > %t.o
// RUN: env TMP="%t/fake/" TMPDIR="%t/fake/" not %target-build-swift -driver-filelist-threshold=0 %t.o 2>&1 | %FileCheck -check-prefix=CHECK-FILELIST %s

// CHECK-FILELIST: - unable to create temporary file for inputs.LinkFileList

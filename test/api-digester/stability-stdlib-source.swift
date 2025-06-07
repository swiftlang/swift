// REQUIRES: OS=macosx

// https://github.com/apple/swift/issues/55803
// This currently fails on non-Intel architectures due to no baseline being
// available and it is not possible to filter across architectures in the
// output.
// XFAIL: CPU=arm64e

// RUN: %empty-directory(%t.tmp)
// mkdir %t.tmp/module-cache && mkdir %t.tmp/dummy.sdk
// RUN: %api-digester -diagnose-sdk -module Swift -o %t.tmp/changes.txt -module-cache-path %t.tmp/module-cache -sdk %t.tmp/dummy.sdk -avoid-location
// RUN: %clang -E -P -x c %S/Outputs/stability-stdlib-source-%target-cpu.swift.expected -o - | sed '/^\s*$/d' | sort > %t.tmp/stability-stdlib-source.swift.expected
// RUN: %clang -E -P -x c %t.tmp/changes.txt -o - | sed '/^\s*$/d' | sort > %t.tmp/changes.txt.tmp
// RUN: diff -u %t.tmp/stability-stdlib-source.swift.expected %t.tmp/changes.txt.tmp


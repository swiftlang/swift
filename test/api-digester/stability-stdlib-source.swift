// REQUIRES: OS=macosx
// RUN: %empty-directory(%t.tmp)
// mkdir %t.tmp/module-cache && mkdir %t.tmp/dummy.sdk
// RUN: %api-digester -dump-sdk -module Swift -o %t.tmp/current-stdlib.json -module-cache-path %t.tmp/module-cache -sdk %t.tmp/dummy.sdk -avoid-location
// RUN: %api-digester -diagnose-sdk -input-paths %S/Inputs/stdlib-stable.json -input-paths %t.tmp/current-stdlib.json -o %t.tmp/changes.txt
// RUN: %clang -E -P -x c %S/Outputs/stability-stdlib-source.swift.expected -o - | sed '/^\s*$/d' | sort > %t.tmp/stability-stdlib-source.swift.expected
// RUN: %clang -E -P -x c %t.tmp/changes.txt -o - | sed '/^\s*$/d' | sort > %t.tmp/changes.txt.tmp
// RUN: diff -u %t.tmp/stability-stdlib-source.swift.expected %t.tmp/changes.txt.tmp

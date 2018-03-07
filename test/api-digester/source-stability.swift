// REQUIRES: OS=macosx
// RUN: %empty-directory(%t.tmp)
// mkdir %t.tmp/module-cache && mkdir %t.tmp/dummy.sdk
// RUN: %api-digester -dump-sdk -module Swift -o %t.tmp/current-stdlib.json -module-cache-path %t.tmp/module-cache -sdk %t.tmp/dummy.sdk -swift-version 3
// RUN: %api-digester -diagnose-sdk -input-paths %S/stdlib-stable.json -input-paths %t.tmp/current-stdlib.json >> %t.tmp/changes.txt
// RUN: %clang -E -P -x c %S/source-stability.swift.expected -o - | sed '/^\s*$/d' > %t.tmp/source-stability.swift.expected
// RUN: %clang -E -P -x c %t.tmp/changes.txt -o - | sed '/^\s*$/d' > %t.tmp/changes.txt.tmp
// RUN: diff -u %t.tmp/source-stability.swift.expected %t.tmp/changes.txt.tmp ; rm -rf %S/tmp

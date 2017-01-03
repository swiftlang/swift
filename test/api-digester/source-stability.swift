// REQUIRES: OS=macosx
// RUN: rm -rf %S/tmp && mkdir %S/tmp && mkdir %S/tmp/module-cache && mkdir %S/tmp/dummy.sdk
// RUN: %api-digester -dump-sdk -module Swift -o %S/tmp/current-stdlib.json -module-cache-path %S/tmp/module-cache -sdk %S/tmp/dummy.sdk -swift-version 3
// RUN: %api-digester -diagnose-sdk -input-paths %S/stdlib-stable.json -input-paths %S/tmp/current-stdlib.json >> %S/tmp/changes.txt
// RUN: %clang -E -P -x c %S/source-stability.swift.expected -o - | sed '/^\s*$/d' > %S/tmp/source-stability.swift.expected
// RUN: %clang -E -P -x c %S/tmp/changes.txt -o - | sed '/^\s*$/d' > %S/tmp/changes.txt.tmp
// RUN: diff -u %S/tmp/source-stability.swift.expected %S/tmp/changes.txt.tmp
// RUN: rm -rf %S/tmp

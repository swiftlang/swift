// RUN: rm -rf %t
// RUN: %swift-syntax-test -input-source-filename %s -parse-gen > %t
// RUN: diff -u %s %t
// RUN: %swift-syntax-test -input-source-filename %s -parse-gen -print-node-kind > %t.withkinds
// RUN: diff -u %S/Outputs/round_trip_invalid.swift.withkinds %t.withkinds
// RUN: %swift-syntax-test -input-source-filename %s -eof > %t
// RUN: diff -u %s %t
// RUN: %swift-syntax-test -serialize-raw-tree -input-source-filename %s > %t.dump
// RUN: %swift-syntax-test -deserialize-raw-tree -input-source-filename %t.dump -output-filename %t
// RUN: diff -u %s %t

// Function body without closing brace token.
func foo() {
  var a = 2

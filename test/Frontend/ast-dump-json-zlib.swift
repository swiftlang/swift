// This test verifies that the zlib-compressed JSON option returns something
// that looks like correct JSON once it's been decompressed. Note that the
// output is a simple zlib-compressed stream and *not* a gzip archive.

// Windows does not currently build with zlib support.
// UNSUPPORTED: OS=windows-msvc

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -target %target-swift-5.9-abi-triple -swift-version 6 -I %S/Inputs/dependencies -parse-as-library -dump-ast -dump-ast-format json-zlib %S/ast-dump-json-no-crash.swift -module-name main -o - > %t/main.jsonz
// RUN: %{python} -c 'import sys, zlib; sys.stdout.write(zlib.decompress(sys.stdin.buffer.read()).decode())' < %t/main.jsonz | %FileCheck %s

// CHECK:      {"_kind":"source_file",
// CHECK-SAME: "items":[{

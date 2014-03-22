// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift %S/Inputs/duplicate_normalprotocolconformance_input.swift -o %t/Swift.swiftmodule -sil-serialize-all -emit-module -parse-stdlib -parse-as-library -module-link-name swift_stdlib_core -module-name Swift
// RUN: %swift -c %s -I=%t -sil-link-all -o %t/out

import Swift

var s = S()


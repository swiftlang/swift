// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift %S/Inputs/duplicate_normalprotocolconformance_input.swift -o %t/Swift.swiftmodule -sil-serialize-all -emit-module -parse-stdlib -parse-as-library -module-link-name swiftCore -module-name Swift
// RUN: %swift -c %s -I=%t -sil-link-all -o %t/out
// XFAIL: linux

import Swift

var s = S()


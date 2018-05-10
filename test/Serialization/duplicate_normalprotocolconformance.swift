// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/Inputs/duplicate_normalprotocolconformance_input.swift -o %t/Swift.swiftmodule -emit-module -parse-stdlib -parse-as-library -module-link-name swiftCore -module-name Swift
// RUN: %target-swift-frontend -c %s -I %t -o %t/out

import Swift

var s = S()


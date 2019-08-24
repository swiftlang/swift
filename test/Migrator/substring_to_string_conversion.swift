// RUN: %empty-directory(%t) && %target-swift-frontend -c -update-code -primary-file %S/Inputs/substring_to_string_conversion.swift -emit-migrated-file-path %t/substring_to_string_conversion.swift.result -swift-version 4
// RUN: diff -u %S/substring_to_string_conversion.swift.expected %t/substring_to_string_conversion.swift.result
// XFAIL: *

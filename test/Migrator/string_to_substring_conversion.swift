// RUN: rm -rf %t && mkdir -p %t && %target-swift-frontend -c -update-code -primary-file %S/Inputs/string_to_substring_conversion.swift -emit-migrated-file-path %t/string_to_substring_conversion.swift.result -swift-version 4
// RUN: diff -u %S/string_conversion.swift.expected %t/string_to_substring_conversion.swift.result
// XFAIL: *

// RUN: %target-swift-frontend -dump-ast %s 2>&1 | %FileCheck %s

import Swift

// ===---------- Raw mode unescaping strings --------===

_ = r"ab\c\r\n\(var)\"

// CHECK: "ab\\c\\r\\n\\(var)\\"

_ = r"""
    "One"\r\n
    \(var)
    """

// CHECK: "\"One\"\\r\\n\n\\(var)"

_ = r"""
    One\
    Two\
    """

// CHECK: "One\\\nTwo\\"

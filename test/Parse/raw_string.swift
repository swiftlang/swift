// RUN: %target-swift-frontend -dump-ast %s 2>&1 | %FileCheck %s

import Swift

// ===---------- Multiline RawString --------===

_ = ##"""
    One
    ""Alpha""
    """##
// CHECK: "One\n\"\"Alpha\"\""

_ = ##"""
    Two
  Beta
  """##
// CHECK: "  Two\nBeta"

_ = \"""
    Three\r
    Gamma\
  """
// CHECK: "  Three\\r\n  Gamma\\"

_ = \###"""
    Four \(foo)
    Delta
"""###
// CHECK: "    Four \\(foo)\n    Delta"

_ = ##"""
  print("""
    Five\n\n\nEpsilon
    """)
  """##
// CHECK: "print(\"\"\"\n  Five\n\n\nEpsilon\n  \"\"\")"

// ===---------- Single line --------===

_ = #""Zeta""#
// CHECK: "\"Zeta\""

_ = #""Eta"\n\n\n\""#
// CHECK: "\"Eta\"\n\n\n\"

_ = \#""Iota"\n\n\n\""#
// CHECK: "\"Iota\"\\n\\n\\n\\\""

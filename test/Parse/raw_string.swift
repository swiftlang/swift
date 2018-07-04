// RUN: %target-swift-frontend -dump-ast %s 2>&1 | %FileCheck %s

import Swift

// ===---------- Multiline RawString --------===

print(##"""
    One
    ""Alpha""
    """##)
// CHECK: "One\n\"\"Alpha\"\""

print(##"""
    Two
  Beta
  """##)
// CHECK: "  Two\nBeta"

print(#"""
    Three\r
    Gamma\
  """#)
// CHECK: "  Three\\r\n  Gamma\\"

print(###"""
    Four \(foo)
    Delta
"""###)
// CHECK: "    Four \\(foo)\n    Delta"

print(##"""
  print("""
    Five\##n\##n\##nEpsilon
    """)
  """##)
// CHECK: "print(\"\"\"\n  Five\n\n\nEpsilon\n  \"\"\")"

// ===---------- Single line --------===

print(#""Zeta""#)
// CHECK: "\"Zeta\""

print(#""Eta"\#n\#n\#n\#""#)
// CHECK: "\"Eta\"\n\n\n\""

print(#""Iota"\n\n\n\""#)
// CHECK: "\"Iota\"\\n\\n\\n\\\""

let foo = "Interpolation"
print(#"\b\b \#(foo)\#(foo) Kappa"#)
// CHECK: "\\b\\b "
// CHECK: " Kappa"

// ===---------- From proposal --------===

_ = #"This is a string"#
// CHECK: "This is a string"

_ = #####"This is a string"#####
// CHECK: "This is a string"

_ = #"enum\s+.+\{.*case\s+[:upper:]"#
// CHECK: "enum\\s+.+\\{.*case\\s+[:upper:]"

_ = #"Alice: "How long is forever?" White Rabbit: "Sometimes, just one second.""#
// CHECK: "Alice: \"How long is forever?\" White Rabbit: \"Sometimes, just one second.\""

_ = #"\#\#1"#
/// CHECK: "\\#1"

_ = ##"\#1"##
/// CHECK: "\\#1"

_ = #"c:\windows\system32"#
/// CHCECK: "c:\\windows\\system32"

_ = #"\d{3) \d{3} \d{4}"#
///CHECK: "\\d{3) \\d{3} \\d{4}"

_ = #"""
    a string with
    """
    in it
    """#
/// CHECK: "a string with\n\"\"\"\nin it"

_ = #"a raw string containing \r\n"#
/// CHECK "a raw string containing \\r\\n"

_ = #"""
    [
        {
            "id": "12345",
            "title": "A title that \"contains\" \\\""
        }
    ]
    """#
/// CHECK: "[\n    {\n        \"id\": \"12345\",\n        \"title\": \"A title that \\\"contains\\\" \\\\\\\"\"\n    }\n]"

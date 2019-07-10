// RUN: %target-swift-frontend -dump-ast %s | %FileCheck --strict-whitespace %s

import Swift

_ = #"""
###################################################################
## This source file is part of the Swift.org open source project ##
###################################################################
"""#
// CHECK: "###################################################################\n## This source file is part of the Swift.org open source project ##\n###################################################################"

_ = #"""
    # H1 #
    ## H2 ##
    ### H3 ###
    """#
// CHECK: "# H1 #\n## H2 ##\n### H3 ###"

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

_ = #"""
    Three\r
    Gamma\
  """#
// CHECK: "  Three\\r\n  Gamma\\"

_ = ###"""
    Four \(foo)
    Delta
"""###
// CHECK: "    Four \\(foo)\n    Delta"

_ = ##"""
  print("""
    Five\##n\##n\##nEpsilon
    """)
  """##
// CHECK: "print(\"\"\"\n  Five\n\n\nEpsilon\n  \"\"\")"

// ===---------- Single line --------===

_ = #""Zeta""#
// CHECK: "\"Zeta\""

_ = #""Eta"\#n\#n\#n\#""#
// CHECK: "\"Eta\"\n\n\n\""

_ = #""Iota"\n\n\n\""#
// CHECK: "\"Iota\"\\n\\n\\n\\\""

_ = #"a raw string with \" in it"#
// CHECK: "a raw string with \\\" in it"

_ = ##"""
      a raw string with """ in it
      """##
// CHECK: "a raw string with \"\"\" in it"

let foo = "Interpolation"
_ = #"\b\b \#(foo)\#(foo) Kappa"#
// CHECK: "\\b\\b "
// CHECK: " Kappa"

_ = """
  interpolating \(##"""
    delimited \##("string")\#n\##n
    """##)
  """

// CHECK: "interpolating "
// CHECK: "delimited "
// CHECK: "string"
// CHECK: "\\#n\n"

#"unused literal"#
// CHECK: "unused literal"

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
// CHECK: "\\#1"

_ = ##"\#1"##
// CHECK: "\\#1"

_ = #"c:\windows\system32"#
// CHECK: "c:\\windows\\system32"

_ = #"\d{3) \d{3} \d{4}"#
// CHECK: "\\d{3) \\d{3} \\d{4}"

_ = #"""
    a string with
    """
    in it
    """#
// CHECK: "a string with\n\"\"\"\nin it"

_ = #"a raw string containing \r\n"#
// CHECK: "a raw string containing \\r\\n"

_ = #"""
    [
        {
            "id": "12345",
            "title": "A title that \"contains\" \\\""
        }
    ]
    """#
// CHECK: "[\n    {\n        \"id\": \"12345\",\n        \"title\": \"A title that \\\"contains\\\" \\\\\\\"\"\n    }\n]"

_ = #"# #"#
// CHECK: "# #"

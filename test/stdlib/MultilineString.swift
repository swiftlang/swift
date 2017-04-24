// RUN: %target-run-stdlib-swift | %FileCheck %s
// REQUIRES: executable_test

import Swift

// ===---------- Multiline --------===

func delimit(_ str: String) -> String {
  return "<\(str)>"
}

// CHECK: -1-
print("-1-")
// CHECK-NEXT: {{^}}<One
// CHECK-NEXT: {{^}}""Alpha"">
print(delimit("""
    One
    ""Alpha""
    """
))

// CHECK: -2-
print("-2-")
// CHECK-NEXT: {{^}}<  Two
// CHECK-NEXT: {{^}}Beta>
print(delimit("""
    Two
  Beta
  """
))

// CHECK: -3-
print("-3-")
// CHECK-NEXT: {{^}}<  Three
// CHECK-NEXT: {{^}}  Gamma>
print(delimit("""
    Three
    Gamma
  """
))

// CHECK: -4-
print("-4-")
// CHECK-NEXT: {{^}}<    Four
// CHECK-NEXT: {{^}}    Delta>
print(delimit("""
    Four
    Delta
"""))

// CHECK: -5-
print("-5-")
// CHECK-NEXT: {{^}}<Five
// CHECK: {{^}}
// CHECK: {{^}}
// CHECK: {{^}}Epsilon>
print(delimit("""
    Five\n

    Epsilon
    """))

// CHECK: -6-
print("-6-")
// CHECK-NEXT: {{^}}<Six
// CHECK-NEXT: {{^}}Zeta
// CHECK-NEXT: {{^}}>
print(delimit("""
    Six
    Zeta

    """))

// CHECK: -7-
print("-7-")
// CHECK-NEXT: {{^}}<Seven
// CHECK-NEXT: {{^}}Eta
// CHECK-NEXT: {{^}}>
print(delimit("""
    Seven
    Eta\n
    """))

// CHECK: -8-
print("-8-")
// CHECK-NEXT: {{^}}<"""
// CHECK-NEXT: {{^}}"""
// CHECK-NEXT: {{^}}"""
// CHECK-NEXT: {{^}}Iota>
print(delimit("""
    \"""
    "\""
    ""\"
    Iota
    """))

// CHECK: -9-
print("-9-")
// CHECK-NEXT: {{^}}< Nine
// CHECK-NEXT: {{^}}Kappa>
print(delimit("""
     \("Nine")
    Kappa
    """))

// CHECK: -10-
print("-10-")
// CHECK-NEXT: {{^}}<\>
print(delimit("""
\\
"""))

// CHECK: -11-
print("-11-")
// CHECK-NEXT: {{^}}<
// CHECK-NEXT: {{^}}ABC>
print(delimit("""

  ABC
  """))

// ===---------- Done --------===
// CHECK-NEXT: Done.
print("Done.")

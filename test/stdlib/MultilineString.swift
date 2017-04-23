// RUN: %target-run-stdlib-swift | %FileCheck %s
// REQUIRES: executable_test

// RUN: %target-build-swift %s 2> %t.warnings.txt
// RUN: %FileCheck -check-prefix=CHECK-WARNINGS %s < %t.warnings.txt

import Swift

// ===---------- Multiline --------===

func delimit(_ str: String) -> String {
  return "<\(str)>"
}

// CHECK: -1-
print("-1-")
// CHECK-NEXT: {{^}}<Six
// CHECK-NEXT: {{^}}""Zeta"">
print(delimit("""
    Six
    ""Zeta""
    """
))

// CHECK: -2-
print("-2-")
// CHECK-NEXT: {{^}}<  Eight
// CHECK-NEXT: {{^}}Iota>
print(delimit("""
    Eight
  Iota
  """
))

// CHECK: -3-
print("-3-")
// CHECK-NEXT: {{^}}<  Nine
// CHECK-NEXT: {{^}}  Kappa>
print(delimit("""
    Nine
    Kappa
  """
))

// CHECK: -4-
print("-4-")
// CHECK-NEXT: {{^}}<    Ten
// CHECK-NEXT: {{^}}    Lambda>
print(delimit("""
    Ten
    Lambda
"""))

// ===---------- Done --------===
// CHECK-NEXT: Done.
print("Done.")

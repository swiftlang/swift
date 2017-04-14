// RUN: %target-run-stdlib-swift | %FileCheck %s
// REQUIRES: executable_test

// RUN: %target-build-swift %s 2> %t.warnings.txt
// RUN: %FileCheck -check-prefix=CHECK-WARNINGS %s < %t.warnings.txt

import Foundation
import Swift

// ===---------- Multiline --------===

func delimit(_ str: String) -> String {
  return "<\(str)>"
}

// CHECK: -1-
print("-1-")
// CHECK-NEXT: <One Alpha>
print(delimit("""One Alpha"""))

// CHECK: -2-
print("-2-")
// SKIP-CHECK-NEXT: <"Two Beta">
//print(delimit(""""Two Beta""""))

// CHECK: -3-
print("-3-")
// CHECK-NEXT: <Three
// CHECK-NEXT: Gamma> 
print(delimit("""Three
Gamma"""))

// CHECK: -4-
print("-4-")
// CHECK-NEXT: <FourDelta>
print(delimit("""Four\
Delta"""))

// CHECK: -5-
print("-5-")
// CHECK-NEXT: <Five
// CHECK-NEXT: Epsilon
// CHECK-NEXT: >
print(delimit("""Five
Epsilon\n"""))

// CHECK: -6-
print("-6-")
// CHECK-NEXT: {{^}}<Six
// CHECK-NEXT: {{^}}Zeta
// CHECK-NEXT: {{^}}>
print(delimit("""
    Six
    Zeta
    """
))

// CHECK: -7-
print("-7-")
// CHECK-NEXT: {{^}}<Seven
// CHECK-NEXT: {{^}}Eta>
print(delimit("""
    Seven
    Eta\
    """
))

// CHECK: -8-
print("-8-")
// CHECK-NEXT: {{^}}<  Eight
// CHECK-NEXT: {{^}}Iota
// CHECK-NEXT: {{^}}>
print(delimit("""
    Eight
  Iota
  """
))

// CHECK: -9-
print("-9-")
// CHECK-NEXT: {{^}}<  Nine
// CHECK-NEXT: {{^}}  Kappa
// CHECK-NEXT: {{^}}>
print(delimit("""
    Nine
    Kappa
  """
))

// CHECK: -10-
print("-10-")
// CHECK-NEXT: {{^}}<    Ten
// CHECK-NEXT: {{^}}    Lambda
// CHECK-NEXT: {{^}}>
print(delimit("""
    Ten
    Lambda
"""))

// CHECK: -11-
print("-11-")
// CHECK-WARNINGS: warning: invalid mix of multi-line string literal indentation
// CHECK-NEXT: {{^}}<Eleven
// CHECK-NEXT: {{^}}  Mu
// CHECK-NEXT: {{^}}>
print(delimit("""
    Eleven
  Mu
    """
))

// CHECK: -12-
print("-12-")
// Note: The next few tests use physical tab characters, not spaces.
// CHECK-WARNINGS: warning: invalid mix of multi-line string literal indentation
// CHECK-NEXT: {{^}}<Twelve
// CHECK-NEXT: {{^}}	Nu
// CHECK-NEXT: {{^}}>
print(delimit("""
	Twelve
\tNu
	"""
))

// CHECK: -13-
print("-13-")
// CHECK-WARNINGS: warning: invalid mix of multi-line string literal indentation
// CHECK-NEXT: {{^}}<Thirteen
// CHECK-NEXT: {{^}}	Xi
// CHECK-NEXT: {{^}}>
print(delimit("""
  Thirteen
	Xi
  """
))

// CHECK: -14-
print("-14-")
// CHECK-WARNINGS: warning: invalid mix of multi-line string literal indentation
// CHECK-NEXT: {{^}}<Fourteen
// CHECK-NEXT: {{^}}  	Pi
// CHECK-NEXT: {{^}}>
print(delimit("""
    Fourteen
  	Pi
    """
))
// Okay, we're done with tabs.

// CHECK: -15-
print("-15-")
// CHECK-NEXT: {{^}}<Fifteen
// CHECK-NEXT: {{^}}Rho
// CHECK-NEXT: {{^}}>
print(delimit("""\
    Fifteen
    Rho
    """
))

// CHECK: -16-
print("-16-")
// Note: There are trailing spaces on lines in this test which must not be removed.
// FIXME: We also want to check that this emits a warning on the delimiter line.
// CHECK-NEXT: {{^}}<    
// CHECK-NEXT: {{^}}Sixteen
// CHECK-NEXT: {{^}}Sigma
// CHECK-NEXT: {{^}}>
print(delimit("""    
    Sixteen
    Sigma
    """
))

// CHECK: -17-
print("-17-")
// CHECK-NEXT: {{^}}<    Seventeen
// CHECK-NEXT: {{^}}    Tau>
print(delimit("""
    Seventeen
    Tau"""
))

// Finally, let's try some syntaxes which should create empty strings:

// CHECK: -18-
print("-18-")
// CHECK-NEXT: 1:<>
// CHECK-NEXT: 2:<>
// CHECK-NEXT: 3:<>
// CHECK-NEXT: 4:<>
print("1:" + delimit(""""""))
print("2:" + delimit("""\
"""))
print("3:" + delimit("""
"""))
print("4:" + delimit("""
    """
))

// CHECK: -11-
print("-11-")
// CHECK-NEXT: <NineteenUpsilon>
print(delimit("""Nineteen\
Upsilon"""))

// ===---------- Done --------===
// CHECK-NEXT: Done.
print("Done.")


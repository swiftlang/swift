// RUN: %target-build-swift %s 2> %t.warnings.txt || echo -n
// RUN: %FileCheck -check-prefix=CHECK-ERRORS %s < %t.warnings.txt

import Swift

// ===---------- Multiline --------===

func delimit(_ str: String) -> String {
    return "<\(str)>"
}

// CHECK: -1-
print("-1-")
// CHECK-ERRORS: error: invalid mix of multi-line string literal indentation
print(delimit("""
    Eleven
  Mu
    """
))

// CHECK: -2-
print("-2-")
// Note: The next few tests use physical tab characters, not spaces.
// CHECK-ERRORS: error: invalid mix of multi-line string literal indentation
print(delimit("""
	Twelve
\tNu
	"""
))

// CHECK: -3-
print("-3-")
// CHECK-ERRORS: error: invalid mix of multi-line string literal indentation
print(delimit("""
  Thirteen
	Xi
  """
))

// CHECK: -4-
print("-4-")
// CHECK-ERRORS: error: invalid mix of multi-line string literal indentation
print(delimit("""
    Fourteen
  	Pi
    """
))

// CHECK: -5-
print("-5-")
// CHECK-ERRORS: error: invalid start of multi-line string literal
print(delimit("""Fourteen
    Pi
    """
))

// CHECK: -6-
print("-6-")
// CHECK-ERRORS: error: invalid end of multi-line string literal
print(delimit("""
    Fourteen
    Pi"""
))

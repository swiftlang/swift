// RUN: %target-build-swift %s 2> %t.warnings.txt || echo -n
// RUN: %FileCheck -check-prefix=CHECK-ERRORS %s < %t.warnings.txt

import Swift

// ===---------- Multiline --------===

func delimit(_ str: String) -> String {
    return "<\(str)>"
}

// CHECK: -11-
print("-11-")
// CHECK-ERRORS: error: invalid mix of multi-line string literal indentation
print(delimit("""
    Eleven
  Mu
    """
))

// CHECK: -12-
print("-12-")
// Note: The next few tests use physical tab characters, not spaces.
// CHECK-ERRORS: error: invalid mix of multi-line string literal indentation
print(delimit("""
	Twelve
\tNu
	"""
))

// CHECK: -13-
print("-13-")
// CHECK-ERRORS: error: invalid mix of multi-line string literal indentation
print(delimit("""
  Thirteen
	Xi
  """
))

// CHECK: -14-
print("-14-")
// CHECK-ERRORS: error: invalid mix of multi-line string literal indentation
print(delimit("""
    Fourteen
  	Pi
    """
))

// CHECK: -15-
print("-15-")
// CHECK-ERRORS: error: invalid start of multi-line string literal
print(delimit("""Fourteen
    Pi
    """
))

// CHECK: -16-
print("-16-")
// CHECK-ERRORS: error: invalid end of multi-line string literal
print(delimit("""
    Fourteen
    Pi"""
))

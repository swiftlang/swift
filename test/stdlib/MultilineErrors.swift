// RUN: %target-build-swift %s 2> %t.warnings.txt
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

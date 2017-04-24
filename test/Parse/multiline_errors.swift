// RUN: %target-typecheck-verify-swift

import Swift

// ===---------- Multiline --------===

_ = """
    Eleven
  Mu
    """ // expected-error@-1{{invalid mix of multi-line string literal indentation}}

_ = """
	Twelve
\tNu
	""" // expected-error@-1{{invalid mix of multi-line string literal indentation}}

_ = """
  Thirteen
	Xi
  """ // expected-error@-1{{invalid mix of multi-line string literal indentation}}

_ = """
    Fourteen
  	Pi
    """ // expected-error@-1{{invalid mix of multi-line string literal indentation}}

_ = """Fourteen
    Pi
    """ // expected-error@-2{{invalid start of multi-line string literal}}

_ = """
    Fourteen
    Pi""" // expected-error@-0{{invalid end of multi-line string literal}}

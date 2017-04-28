// RUN: %target-typecheck-verify-swift

import Swift

// ===---------- Multiline --------===

_ = """
    Eleven
  Mu
    """ // expected-error@-1{{invalid mix of multi-line string literal indentation}}
// expecting at least 4 columns of leading indentation

_ = """
    Eleven
   Mu
    """ // expected-error@-1{{invalid mix of multi-line string literal indentation}}
// expecting at least 4 columns of leading indentation

_ = """
	Twelve
\tNu
	""" // expected-error@-1{{invalid mix of multi-line string literal indentation}}
// \t is not the same as an actual tab for de-indentation

_ = """
  Thirteen
	Xi
  """ // expected-error@-1{{invalid mix of multi-line string literal indentation}}
// a tab is not the same as multiple spaces for de-indentation

_ = """
    Fourteen
  	Pi
    """ // expected-error@-1{{invalid mix of multi-line string literal indentation}}
// a tab is not the same as multiple spaces for de-indentation

_ = """Fourteen
    Pi
    """ // expected-error@-2{{invalid start of multi-line string literal}}
// newline currently required after opening """

_ = """
    Fourteen
    Pi""" // expected-error@-0{{invalid end of multi-line string literal}}
// newline currently required before closing """

_ = """""" // expected-error@-0{{invalid start of multi-line string literal}}
// newline currently required after opening """

_ = """ """ // expected-error@-0{{invalid start of multi-line string literal}}
// newline currently required after opening """

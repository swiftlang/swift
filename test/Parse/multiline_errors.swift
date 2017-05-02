// RUN: %target-typecheck-verify-swift

import Swift

// ===---------- Multiline --------===

// expecting at least 4 columns of leading indentation
_ = """
    Eleven
  Mu
    """ // expected-error@-1{{line does not start with same indentation as closing delimiter of multi-line string literal}}
        // expected-note@-2{{unexpectedly found non-indentation character here}}
        // expected-note@-2{{should match space in same position before closing delimiter}}
        // expected-note@-4{{change indentation of this line to match closing delimiter}} {{1-3=    }}

// expecting at least 4 columns of leading indentation
_ = """
    Eleven
   Mu
    """ // expected-error@-1{{line does not start with same indentation as closing delimiter of multi-line string literal}}
        // expected-note@-2{{unexpectedly found non-indentation character here}}
        // expected-note@-2{{should match space in same position before closing delimiter}}
        // expected-note@-4{{change indentation of this line to match closing delimiter}} {{1-4=    }}

// \t is not the same as an actual tab for de-indentation
_ = """
	Twelve
\tNu
	""" // expected-error@-1{{line does not start with same indentation as closing delimiter of multi-line string literal}}
      // expected-note@-2{{unexpectedly found non-indentation character here}}
      // expected-note@-2{{should match tab in same position before closing delimiter}}
      // expected-note@-4{{change indentation of this line to match closing delimiter}} {{1-1=	}}

// a tab is not the same as multiple spaces for de-indentation
_ = """
  Thirteen
	Xi
  """ // expected-error@-1{{line does not start with same indentation as closing delimiter of multi-line string literal}}
      // expected-note@-2{{unexpected tab in indentation here}}
      // expected-note@-2{{should match space in same position before closing delimiter}}
      // expected-note@-4{{change indentation of this line to match closing delimiter}} {{1-2=  }}

// a tab is not the same as multiple spaces for de-indentation
_ = """
    Fourteen
  	Pi
    """ // expected-error@-1{{line does not start with same indentation as closing delimiter of multi-line string literal}}
        // expected-note@-2{{unexpected tab in indentation here}}
        // expected-note@-2{{should match space in same position before closing delimiter}}
        // expected-note@-4{{change indentation of this line to match closing delimiter}} {{1-4=    }}

// multiple spaces are not the same as a tab for de-indentation
_ = """
	Thirteen 2
  Xi 2
	""" // expected-error@-1{{line does not start with same indentation as closing delimiter of multi-line string literal}}
      // expected-note@-2{{unexpected space in indentation here}}
      // expected-note@-2{{should match tab in same position before closing delimiter}}
      // expected-note@-4{{change indentation of this line to match closing delimiter}} {{1-3=	}}

// multiple spaces are not the same as a tab for de-indentation
_ = """
		Fourteen 2
	  Pi 2
		""" // expected-error@-1{{line does not start with same indentation as closing delimiter of multi-line string literal}}
        // expected-note@-2{{unexpected space in indentation here}}
        // expected-note@-2{{should match tab in same position before closing delimiter}}
        // expected-note@-4{{change indentation of this line to match closing delimiter}} {{1-4=		}}

// newline currently required after opening """
_ = """Fourteen
    Pi
    """ // expected-error@-2{{multi-line string literal content must begin on a new line}} {{8-8=\n}}

// newline currently required before closing """
_ = """
    Fourteen
    Pi""" // expected-error@-0{{multi-line string literal closing delimiter must begin on a new line}} {{7-7=\n}}

// newline currently required after opening """
_ = """""" // expected-error@-0{{multi-line string literal content must begin on a new line}} {{8-8=\n}}

// newline currently required after opening """
_ = """ """ // expected-error@-0{{multi-line string literal content must begin on a new line}} {{8-8=\n}}

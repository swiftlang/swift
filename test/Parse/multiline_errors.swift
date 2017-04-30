// RUN: %target-typecheck-verify-swift

import Swift

// ===---------- Multiline --------===

// expecting at least 4 columns of leading indentation
_ = """
    Eleven
  Mu
    """ // expected-error@-1{{string literal content is insufficiently indented}}
        // expected-note@-2{{change indentation to match last line}} {{1-3=    }}
        // expected-note@-2{{change last line's indentation to match this line}} {{1-5=  }}

// expecting at least 4 columns of leading indentation
_ = """
    Eleven
   Mu
    """ // expected-error@-1{{string literal content is insufficiently indented}}
        // expected-note@-2{{change indentation to match last line}} {{1-4=    }}
        // expected-note@-2{{change last line's indentation to match this line}} {{1-5=   }}

// \t is not the same as an actual tab for de-indentation
_ = """
	Twelve
\tNu
	""" // expected-error@-1{{string literal content is insufficiently indented}}
      // expected-note@-2{{change indentation to match last line}} {{1-1=	}}
      // expected-note@-2{{change last line's indentation to match this line}} {{1-2=}}

// a tab is not the same as multiple spaces for de-indentation
_ = """
  Thirteen
	Xi
  """ // expected-error@-1{{string literal content is indented with a tab where a space is expected}}
      // expected-note@-2{{change indentation to match last line}} {{1-2=  }}
      // expected-note@-2{{change last line's indentation to match this line}} {{1-3=	}}

// a tab is not the same as multiple spaces for de-indentation
_ = """
    Fourteen
  	Pi
    """ // expected-error@-1{{string literal content is indented with a tab where a space is expected}}
        // expected-note@-2{{change indentation to match last line}} {{1-4=    }}
        // expected-note@-2{{change last line's indentation to match this line}} {{1-5=  	}}

// multiple spaces are not the same as a tab for de-indentation
_ = """
	Thirteen 2
  Xi 2
	""" // expected-error@-1{{string literal content is indented with a space where a tab is expected}}
      // expected-note@-2{{change indentation to match last line}} {{1-3=	}}
      // expected-note@-2{{change last line's indentation to match this line}} {{1-2=  }}

// multiple spaces are not the same as a tab for de-indentation
_ = """
		Fourteen 2
	  Pi 2
		""" // expected-error@-1{{string literal content is indented with a space where a tab is expected}}
        // expected-note@-2{{change indentation to match last line}} {{1-4=		}}
        // expected-note@-2{{change last line's indentation to match this line}} {{1-3=	  }}

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

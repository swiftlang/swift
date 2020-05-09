// RUN: %target-typecheck-verify-swift

import Swift

// ===---------- Multiline --------===

// expecting at least 4 columns of leading indentation
_ = """
    Eleven
  Mu
    """ // expected-error@-1{{insufficient indentation of line in multi-line string literal}}
        // expected-note@-1{{should match space here}}
        // expected-note@-3{{change indentation of this line to match closing delimiter}} {{3-3=  }}

// expecting at least 4 columns of leading indentation
_ = """
    Eleven
   Mu
    """ // expected-error@-1{{insufficient indentation of line in multi-line string literal}}
        // expected-note@-1{{should match space here}}
        // expected-note@-3{{change indentation of this line to match closing delimiter}} {{4-4= }}

// \t is not the same as an actual tab for de-indentation
_ = """
	Twelve
\tNu
	""" // expected-error@-1{{insufficient indentation of line in multi-line string literal}}
      // expected-note@-1{{should match tab here}}
      // expected-note@-3{{change indentation of this line to match closing delimiter}} {{1-1=	}}

_ = """
    \(42
)
    """ // expected-error@-1{{insufficient indentation of line in multi-line string literal}}
        // expected-note@-1{{should match space here}}
        // expected-note@-3{{change indentation of this line to match closing delimiter}} {{1-1=    }}

_ = """
    Foo
\
    Bar 
    """ // expected-error@-2{{insufficient indentation of line in multi-line string literal}}
        // expected-note@-1{{should match space here}}
        // expected-note@-4{{change indentation of this line to match closing delimiter}} {{1-1=    }}

// a tab is not the same as multiple spaces for de-indentation
_ = """
  Thirteen
	Xi
  """ // expected-error@-1{{unexpected tab in indentation of line in multi-line string literal}}
      // expected-note@-1{{should match space here}}
      // expected-note@-3{{change indentation of this line to match closing delimiter}} {{1-2=  }}

// a tab is not the same as multiple spaces for de-indentation
_ = """
    Fourteen
  	Pi
    """ // expected-error@-1{{unexpected tab in indentation of line in multi-line string literal}}
        // expected-note@-1{{should match space here}}
        // expected-note@-3{{change indentation of this line to match closing delimiter}} {{3-4=  }}

// multiple spaces are not the same as a tab for de-indentation
_ = """
	Thirteen 2
  Xi 2
	""" // expected-error@-1{{unexpected space in indentation of line in multi-line string literal}}
      // expected-note@-1{{should match tab here}}
      // expected-note@-3{{change indentation of this line to match closing delimiter}} {{1-3=	}}

// multiple spaces are not the same as a tab for de-indentation
_ = """
		Fourteen 2
	  Pi 2
		""" // expected-error@-1{{unexpected space in indentation of line in multi-line string literal}}
        // expected-note@-1{{should match tab here}}
        // expected-note@-3{{change indentation of this line to match closing delimiter}} {{2-4=	}}

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

// two lines should get only one error
_ = """
    Hello,
        World!
	"""     // expected-error@-2{{unexpected space in indentation of next 2 lines in multi-line string literal}}
          // expected-note@-1{{should match tab here}}
          // expected-note@-4{{change indentation of these lines to match closing delimiter}} {{1-5=	}} {{1-5=	}}

  _ = """
Zero A
Zero B
	One A
	One B
  Two A
  Two B
Three A
Three B
		Four A
		Four B
			Five A
			Five B
		"""   // expected-error@-12{{insufficient indentation of next 2 lines in multi-line string literal}}
          // expected-note@-1{{should match tab here}}
          // expected-note@-14{{change indentation of these lines to match closing delimiter}} {{1-1=		}} {{1-1=		}}
          // expected-error@-13{{insufficient indentation of next 2 lines in multi-line string literal}}
          // expected-note@-4{{should match tab here}}
          // expected-note@-15{{change indentation of these lines to match closing delimiter}} {{2-2=	}} {{2-2=	}}
          // expected-error@-14{{unexpected space in indentation of next 4 lines in multi-line string literal}}
          // expected-note@-7{{should match tab here}}
          // expected-note@-16{{change indentation of these lines to match closing delimiter}} {{1-1=		}} {{1-1=		}} {{1-1=		}} {{1-1=		}}

_ = "hello\("""
            world
            """
            )!"
            // expected-error@-4 {{unterminated string literal}}
            // expected-error@-2 {{unterminated string literal}}

_ = "hello\(
            """
            world
            """)!"
            // expected-error@-4 {{unterminated string literal}}
            // expected-error@-2 {{unterminated string literal}}

_ = """
  line one \ non-whitepace
  line two
  """
  // expected-error@-3 {{invalid escape sequence in literal}}

_ = """
  line one
  line two\
  """
  // expected-error@-2 {{escaped newline at the last line is not allowed}} {{11-12=}}

_ = """
  \\\	   
  """
  // expected-error@-2 {{escaped newline at the last line is not allowed}} {{5-10=}}

_ = """
  \(42)\		
  """
  // expected-error@-2 {{escaped newline at the last line is not allowed}} {{8-11=}}

_ = """
  foo\
  """
  // expected-error@-2 {{escaped newline at the last line is not allowed}} {{6-7=}}

_ = """
  foo\  """
  // expected-error@-1 {{escaped newline at the last line is not allowed}} {{6-7=}}

_ = """
  foo\
  """ // OK because LF + CR is two new lines.

_ = """
\
  """
  // expected-error@-2 {{escaped newline at the last line is not allowed}} {{1-2=}}
  // expected-error@-3{{insufficient indentation of line in multi-line string literal}}
  // expected-note@-3{{should match space here}}
  // expected-note@-5{{change indentation of this line to match closing delimiter}} {{1-1=  }}

_ = """\
  """
  // FIXME: Bad diagnostics
  // expected-error@-3 {{multi-line string literal content must begin on a new line}}
  // expected-error@-4 {{escaped newline at the last line is not allowed}} {{8-9=}}

let _ = """
  foo
  \("bar
  baz
  """
  // expected-error@-3 {{unterminated string literal}}

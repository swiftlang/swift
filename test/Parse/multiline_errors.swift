// RUN: %target-typecheck-verify-swift

import Swift

// ===---------- Multiline --------===

// expecting at least 4 columns of leading indentation
_ = """
    Eleven
  Mu
    """ // expected-error@-1{{insufficient indentation of line in multi-line string literal}}
        // expected-cparser-note@-1{{should match space here}} expected-astgen-note@-1 {{should match indentation here}}
        // expected-cparser-note@-3{{change indentation of this line to match closing delimiter}} {{3-3=  }}

// expecting at least 4 columns of leading indentation
_ = """
    Eleven
   Mu
    """ // expected-error@-1{{insufficient indentation of line in multi-line string literal}}
        // expected-cparser-note@-1{{should match space here}} expected-astgen-note@-1 {{should match indentation here}}
        // expected-cparser-note@-3{{change indentation of this line to match closing delimiter}} {{4-4= }}

// \t is not the same as an actual tab for de-indentation
_ = """
	Twelve
\tNu
	""" // expected-error@-1{{insufficient indentation of line in multi-line string literal}}
      // expected-cparser-note@-1{{should match tab here}} expected-astgen-note@-1 {{should match indentation here}}
      // expected-cparser-note@-3{{change indentation of this line to match closing delimiter}} {{1-1=	}}

_ = """
    \(42
)
    """ // expected-error@-1{{insufficient indentation of line in multi-line string literal}}
        // expected-cparser-note@-1{{should match space here}} expected-astgen-note@-1 {{should match indentation here}}
        // expected-cparser-note@-3{{change indentation of this line to match closing delimiter}} {{1-1=    }}

_ = """
    Foo
\
    Bar 
    """ // expected-error@-2{{insufficient indentation of line in multi-line string literal}}
        // expected-cparser-note@-1{{should match space here}} expected-astgen-note@-1 {{should match indentation here}}
        // expected-cparser-note@-4{{change indentation of this line to match closing delimiter}} {{1-1=    }}

// a tab is not the same as multiple spaces for de-indentation
_ = """
  Thirteen
	Xi
  """ // expected-error@-1{{unexpected tab in indentation of line in multi-line string literal}}
      // expected-cparser-note@-1{{should match space here}} expected-astgen-note@-1 {{should match indentation here}}
      // expected-cparser-note@-3{{change indentation of this line to match closing delimiter}} {{1-2=  }}

// a tab is not the same as multiple spaces for de-indentation
_ = """
    Fourteen
  	Pi
    """ // expected-error@-1{{unexpected tab in indentation of line in multi-line string literal}}
        // expected-cparser-note@-1{{should match space here}} expected-astgen-note@-1 {{should match indentation here}}
        // expected-cparser-note@-3{{change indentation of this line to match closing delimiter}} {{3-4=  }}

// multiple spaces are not the same as a tab for de-indentation
_ = """
	Thirteen 2
  Xi 2
	""" // expected-error@-1{{unexpected space in indentation of line in multi-line string literal}}
      // expected-cparser-note@-1{{should match tab here}} expected-astgen-note@-1 {{should match indentation here}}
      // expected-cparser-note@-3{{change indentation of this line to match closing delimiter}} {{1-3=	}}

// multiple spaces are not the same as a tab for de-indentation
_ = """
		Fourteen 2
	  Pi 2
		""" // expected-error@-1{{unexpected space in indentation of line in multi-line string literal}}
        // expected-cparser-note@-1{{should match tab here}} expected-astgen-note@-1 {{should match indentation here}}
        // expected-cparser-note@-3{{change indentation of this line to match closing delimiter}} {{2-4=	}}

// newline currently required after opening """
_ = """Fourteen
    Pi
    """ // expected-cparser-error@-2{{multi-line string literal content must begin on a new line}} {{8-8=\n}} expected-astgen-error@-2{{multi-line string literal content must begin on a new line}} {{8-8=\n    }}

// newline currently required before closing """
_ = """
    Fourteen
    Pi""" // expected-cparser-error@-0{{multi-line string literal closing delimiter must begin on a new line}} {{7-7=\n}} expected-astgen-error@-0{{multi-line string literal closing delimiter must begin on a new line}} {{7-7=\n    }}

// newline currently required after opening """
_ = """""" // expected-cparser-error@-0{{multi-line string literal content must begin on a new line}} {{8-8=\n}} expected-astgen-error@-0{{multi-line string literal closing delimiter must begin on a new line}}

// newline currently required after opening """
_ = """ """ // expected-cparser-error@-0{{multi-line string literal content must begin on a new line}} {{8-8=\n}} expected-astgen-error@-0{{multi-line string literal closing delimiter must begin on a new line}}

// two lines should get only one error
_ = """
    Hello,
        World!
	"""     // expected-cparser-error@-2{{unexpected space in indentation of next 2 lines in multi-line string literal}} expected-astgen-error@-2 {{unexpected space in indentation of the next 2 lines in multi-line string literal}}
          // expected-cparser-note@-1{{should match tab here}} expected-astgen-note@-1 {{should match indentation here}}
          // expected-cparser-note@-4{{change indentation of these lines to match closing delimiter}} {{1-5=	}} {{1-5=	}}

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
		"""   // expected-cparser-error@-12{{insufficient indentation of next 2 lines in multi-line string literal}} expected-astgen-error@-12 {{insufficient indentation of the next 4 lines in multi-line string literal}}
          // expected-cparser-note@-1{{should match tab here}} expected-astgen-note@-1 {{should match indentation here}}
          // expected-cparser-note@-14{{change indentation of these lines to match closing delimiter}} {{1-1=		}} {{1-1=		}}
          // expected-cparser-error@-13{{insufficient indentation of next 2 lines in multi-line string literal}}
          // expected-cparser-note@-4{{should match tab here}} expected-astgen-note@-4 {{should match indentation here}}
          // expected-cparser-note@-15{{change indentation of these lines to match closing delimiter}} {{2-2=	}} {{2-2=	}}
          // expected-cparser-error@-14{{unexpected space in indentation of next 4 lines in multi-line string literal}} expected-astgen-error@-14 {{unexpected space in indentation of the next 2 lines in multi-line string literal}} expected-astgen-error@-12 {{insufficient indentation of the next 2 lines in multi-line string literal}}
          // expected-cparser-note@-7{{should match tab here}} expected-astgen-note@-7 {{should match indentation here}}
          // expected-cparser-note@-16{{change indentation of these lines to match closing delimiter}} {{1-1=		}} {{1-1=		}} {{1-1=		}} {{1-1=		}}

_ = "hello\("""
            world
            """
            )!"
            // expected-cparser-error@-4 {{cannot find ')' to match opening '(' in string interpolation}} expected-astgen-note@-4 {{to match this opening '('}} expected-astgen-note@-4 {{to match this opening '"'}}
            // expected-cparser-error@-5 {{unterminated string literal}} expected-astgen-error@-3 {{expected '"' to end string literal}} expected-astgen-error@-3 {{expected ')' in string literal}}
            // expected-cparser-error@-3 {{unterminated string literal}} expected-astgen-error@-3 {{unexpected code ')!"' in source file}}

_ = "hello\(
            """
            world
            """)!"
            // expected-cparser-error@-4 {{cannot find ')' to match opening '(' in string interpolation}} expected-astgen-error@-4 {{expected '"' to end string literal}} expected-astgen-error@-4 {{expected value and ')' in string literal}} expected-astgen-note@-4 {{to match this opening '"'}}
            // expected-cparser-error@-5 {{unterminated string literal}} expected-astgen-warning@-4 {{string literal is unused}}
            // expected-cparser-error@-3 {{unterminated string literal}} expected-astgen-error@-3 {{unexpected code ')!"' in source file}}

_ = """
  line one \ non-whitespace
  line two
  """
  // expected-error@-3 {{invalid escape sequence in literal}}

_ = """
  line one
  line two\
  """
  // expected-cparser-error@-2 {{escaped newline at the last line is not allowed}} {{11-12=}} expected-astgen-error@-2 {{escaped newline at the last line of a multi-line string literal is not allowed}}

_ = """
  \\\	   
  """
  // expected-cparser-error@-2 {{escaped newline at the last line is not allowed}} {{5-10=}} expected-astgen-error@-2 {{escaped newline at the last line of a multi-line string literal is not allowed}}

_ = """
  \(42)\		
  """
  // expected-cparser-error@-2 {{escaped newline at the last line is not allowed}} {{8-11=}} expected-astgen-error@-2 {{escaped newline at the last line of a multi-line string literal is not allowed}}

_ = """
  foo\
  """
  // expected-cparser-error@-2 {{escaped newline at the last line is not allowed}} {{6-7=}} expected-astgen-error@-2 {{escaped newline at the last line of a multi-line string literal is not allowed}}

_ = """
  foo\  """
  // expected-cparser-error@-1 {{escaped newline at the last line is not allowed}} {{6-7=}} expected-astgen-error@-1 {{escaped newline at the last line of a multi-line string literal is not allowed}}

_ = """
  foo\
  """ // OK because LF + CR is two new lines.

_ = """
\
  """
  // expected-cparser-error@-2 {{escaped newline at the last line is not allowed}} {{1-2=}} expected-astgen-error@-2 {{escaped newline at the last line of a multi-line string literal is not allowed}}
  // expected-cparser-error@-3{{insufficient indentation of line in multi-line string literal}}
  // expected-cparser-note@-3{{should match space here}}
  // expected-cparser-note@-5{{change indentation of this line to match closing delimiter}} {{1-1=  }}

_ = """\
  """
  // FIXME: Bad diagnostics
  // expected-error@-3 {{multi-line string literal content must begin on a new line}}
  // expected-cparser-error@-4 {{escaped newline at the last line is not allowed}} {{8-9=}} expected-astgen-error@-4 {{escaped newline at the last line of a multi-line string literal is not allowed}}

let _ = """
  foo
  \("bar
  baz
  """
  // expected-cparser-error@-3 {{cannot find ')' to match opening '(' in string interpolation}} expected-astgen-error@-3 {{expected '"' to end string literal}} expected-astgen-note@-3 {{to match this opening '"'}} expected-astgen-note@-3 {{to match this opening '('}}
  // expected-cparser-error@-4 {{unterminated string literal}} expected-astgen-error@-3 {{unexpected code 'baz' in string literal}} expected-astgen-error@-3 {{expected ')' in string literal}}


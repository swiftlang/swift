// RUN: %target-swift-frontend -dump-ast %s 2>&1 | %FileCheck %s

import Swift

// ===---------- Multiline --------===

_ = """
    One
    ""Alpha""
    """
// CHECK: "One\n\"\"Alpha\"\""

_ = """
    Two
  Beta
  """
// CHECK: "  Two\nBeta"

_ = """
    Three
    Gamma
  """
// CHECK: "  Three\n  Gamma"

_ = """
    Four
    Delta
"""
// CHECK: "    Four\n    Delta"

_ = """
    Five\n

    Epsilon
    """
// CHECK: "Five\n\n\nEpsilon"


_ = """
    Six
    Zeta

    """
// CHECK: "Six\nZeta\n"

_ = """
    Seven
    Eta\n
    """
// CHECK: "Seven\nEta\n"

_ = """
    \"""
    "\""
    ""\"
    Iota
    """
// CHECK: "\"\"\"\n\"\"\"\n\"\"\"\nIota"

_ = """
     \("Nine")
    Kappa
    """
// CHECK: "\nKappa"

_ = """
	first
	 second
	third
	"""
// CHECK: "first\n second\nthird"

_ = """
 first
 	second
 third
 """
// CHECK: "first\n\tsecond\nthird"

_ = """
\\
"""
// CHECK: "\\"

_ = """
  \\
  """
// CHECK: "\\"

_ = """

  ABC
  """
// CHECK: "\nABC"


_ = """

ABC
"""
// CHECK: "\nABC"

_ = """
  
  ABC
  """
// CHECK: "\nABC"

// contains tabs
_ = """
	Twelve
	Nu
	"""
// CHECK: "Twelve\nNu"

_ = """
  newline \
  elided
  """
// CHECK: "newline elided"

// contains trailing whitepsace
_ = """
  trailing \
  \("""
    substring1 \
    \("""
      substring2 \          
      substring3
      """)
    """) \
  whitepsace
  """
// CHECK: "trailing "
// CHECK: "substring1 "
// CHECK: "substring2 substring3"
// CHECK: " whitepsace"

// contains trailing whitepsace
_ = """
    foo\ 

    bar
    """
// CHECK: "foo\nbar"

// contains trailing whitepsace
_ = """
    foo\ 
    
    bar
    """
// CHECK: "foo\nbar"

_ = """
    foo \
      bar
    """
// CHECK: "foo   bar"

_ = """

  ABC
  """
// CHECK: "\nABC"

_ = """

    ABC

    """
// CHECK: "\nABC\n"

_ = """


    """
// CHECK: "\n"

_ = """

    """
// CHECK: ""

_ = """
    """
// CHECK: ""

_ = "\("""
  \("a" + """
   valid
  """)
  """) literal"
// CHECK: "a"
// CHECK: " valid"
// CHECK: " literal"

_ = "hello\("""
  world
  """)"
// CHECK: "hello"
// CHECK: "world"

_ = """
  hello\("""
     world
     """)
  abc
  """
// CHECK: "hello"
// CHECK: "world"
// CHECK: "\nabc"

_ = "hello\("""
            "world'
            """)abc"
// CHECK: "hello"
// CHECK: "\"world'"
// CHECK: "abc"

_ = """
    welcome
    \(
      /*
        ')' or '"""' in comment.
        """
      */
      "to\("""
           Swift
           """)"
      // ) or """ in comment.
    )
    !
    """
// CHECK: "welcome\n"
// CHECK: "to"
// CHECK: "Swift"
// CHECK: "\n!"

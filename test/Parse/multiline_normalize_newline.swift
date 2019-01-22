// RUN: %target-swift-frontend -dump-parse %s | %FileCheck %s

// CR
_ = """"""
//CHECK: string_literal_expr {{.*}} value=""

_ = """  test  """
//CHECK: string_literal_expr {{.*}} value="test"

// CR+LF
_ = """
    """
//CHECK: string_literal_expr {{.*}} value=""

_ = """
  test
  """
//CHECK: string_literal_expr {{.*}} value="test"

// CR+LF
_ = """
    """
//CHECK: string_literal_expr {{.*}} value=""
_ = """
  test
  test
  """
//CHECK: string_literal_expr {{.*}} value="test\ntest"

// LF+CR
_ = """
    foo
    foo
    """
//CHECK: string_literal_expr {{.*}} value="\nfoo\n\nfoo\n"

// LF+CR+LF
_ = """

    foo

    foo

    """
//CHECK: string_literal_expr {{.*}} value="\nfoo\n\nfoo\n"

// Mixed no-indent.
_ = """
<LF
<LF<CR
<CR+LF
"""
//CHECK: string_literal_expr {{.*}} value="<LF\n<LF\n<CR\n<CR+LF"

// Mixed indent.
_ = """
	 <LF
	 <LF	 <CR
	 <CR+LF
	 """
//CHECK: string_literal_expr {{.*}} value="<LF\n<LF\n<CR\n<CR+LF"

// Empty line CR, CR+LF, LF.
_ = """
   foo


   bar
   """
//CHECK: string_literal_expr {{.*}} value="foo\n\n\n\nbar"

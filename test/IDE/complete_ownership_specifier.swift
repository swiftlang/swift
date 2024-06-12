// RUN: %batch-code-completion

func foo(x: #^FUNC_PARAM?check=SPECIFIER^#) {
  let fn1 = { (x: #CLOSURE_PARAM?check=SPECIFIER#) in }
  let fn2: (#^CLOSURE_PARAM_TY_COMPLETE?check=SPECIFIER^#) -> Void
  let fn3: (#^CLOSURE_PARAM_TY_INCOMPLETE?check=SPECIFIER^#
}

struct S {
  init(x: #^INIT_PARAM?check=SPECIFIER^#) {}
  subscript(x: #SUBSCRIPT_PARAM?check=SPECIFIER#) {}
}

// Don't complete for enum cases.
enum E {
  case e(#^ENUM_CASE_TY?check=SPECIFIER_NOT^#)
  case f(x: #^ENUM_CASE_LABELED_TY?check=SPECIFIER_NOT^#)
}

// Don't complete for a regular variable type.
let x: #^VAR_TY?check=SPECIFIER_NOT^#

// Or for a return type.
func bar() -> #^RETURN_TY?check=SPECIFIER_NOT^# {}

// SPECIFIER-DAG: Keyword[inout]/None: inout; name=inout
// SPECIFIER-DAG: Keyword/None: consuming; name=consuming
// SPECIFIER-DAG: Keyword/None: borrowing; name=borrowing
// SPECIFIER-DAG: Keyword/None: isolated; name=isolated

// SPECIFIER_NOT-NOT: Keyword[inout]/None: inout
// SPECIFIER_NOT-NOT: Keyword/None: consuming
// SPECIFIER_NOT-NOT: Keyword/None: borrowing
// SPECIFIER_NOT-NOT: Keyword/None: isolated

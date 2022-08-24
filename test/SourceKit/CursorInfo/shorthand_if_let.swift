// RUN: %sourcekitd-test -req=cursor -pos=%(line+1):13 %s -- %s | %FileCheck %s --check-prefix=SIMPLE_PARAM -D#FUNCSTART=%(line+1)
func simple(bar: Int?) {
  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):10 %s -- %s | %FileCheck %s --check-prefix=SIMPLE_CAPTURE -D#FUNCSTART=%(line-1)
  if let bar {
    // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):11 %s -- %s | %FileCheck %s --check-prefix=SIMPLE_REF -D#FUNCSTART=%(line-3)
    print(bar)
  }
}

// SIMPLE_PARAM: source.lang.swift.decl.var.parameter ([[#FUNCSTART]]:13-[[#FUNCSTART]]:16)
// SIMPLE_PARAM: Int
// SIMPLE_PARAM: SECONDARY SYMBOLS BEGIN
// There should be no secondary symbols
// SIMPLE_PARAM-NEXT: SECONDARY SYMBOLS END

// SIMPLE_CAPTURE: source.lang.swift.decl.var.local ([[#FUNCSTART+2]]:10-[[#FUNCSTART+2]]:13)
// SIMPLE_CAPTURE: Int
// SIMPLE_CAPTURE: SECONDARY SYMBOLS BEGIN
// SIMPLE_CAPTURE: source.lang.swift.ref.var.local ([[#FUNCSTART]]:13-[[#FUNCSTART]]:16)
// SIMPLE_CAPTURE: Int?

// SIMPLE_REF: source.lang.swift.ref.var.local ([[#FUNCSTART+2]]:10-[[#FUNCSTART+2]]:13)
// SIMPLE_REF: Int
// SIMPLE_REF: SECONDARY SYMBOLS BEGIN
// SIMPLE_REF-NEXT: SECONDARY SYMBOLS END

func doubleNested(bar: Int??) {
  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):10 %s -- %s | %FileCheck %s --check-prefix=DOUBLE_NESTED_FIRST_CAPTURE -D#FUNCSTART=%(line-1)
  if let bar {
    // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):12 %s -- %s | %FileCheck %s --check-prefix=DOUBLE_NESTED_SECOND_CAPTURE -D#FUNCSTART=%(line-3)
    if let bar {
      // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):13 %s -- %s | %FileCheck %s --check-prefix=DOUBLE_NESTED_REF -D#FUNCSTART=%(line-5)
      print(bar)
    }
  }
}

// DOUBLE_NESTED_FIRST_CAPTURE: source.lang.swift.decl.var.local ([[#FUNCSTART+2]]:10-[[#FUNCSTART+2]]:13)
// DOUBLE_NESTED_FIRST_CAPTURE: Int?
// DOUBLE_NESTED_FIRST_CAPTURE: SECONDARY SYMBOLS BEGIN
// DOUBLE_NESTED_FIRST_CAPTURE: source.lang.swift.ref.var.local ([[#FUNCSTART]]:19-[[#FUNCSTART]]:22)
// DOUBLE_NESTED_FIRST_CAPTURE: Int??

// DOUBLE_NESTED_SECOND_CAPTURE: source.lang.swift.decl.var.local ([[#FUNCSTART+4]]:12-[[#FUNCSTART+4]]:15)
// DOUBLE_NESTED_SECOND_CAPTURE: Int
// DOUBLE_NESTED_SECOND_CAPTURE: SECONDARY SYMBOLS BEGIN
// DOUBLE_NESTED_SECOND_CAPTURE: source.lang.swift.ref.var.local ([[#FUNCSTART+2]]:10-[[#FUNCSTART+2]]:13)
// DOUBLE_NESTED_SECOND_CAPTURE: Int?
// DOUBLE_NESTED_SECOND_CAPTURE: source.lang.swift.ref.var.local ([[#FUNCSTART]]:19-[[#FUNCSTART]]:22)
// DOUBLE_NESTED_SECOND_CAPTURE: Int??

// DOUBLE_NESTED_REF: source.lang.swift.ref.var.local ([[#FUNCSTART+4]]:12-[[#FUNCSTART+4]]:15)
// DOUBLE_NESTED_REF: Int
// DOUBLE_NESTED_REF: SECONDARY SYMBOLS BEGIN
// DOUBLE_NESTED_REF-NEXT: SECONDARY SYMBOLS END

// Make sure we don't report secondary symbols if the variable is captured explicitly using '='
func explicitCapture(bar: Int?) {
  // RUN: %sourcekitd-test -req=cursor -pos=%(line+2):10 %s -- %s | %FileCheck %s --check-prefix=EXPLICIT_CAPTURE -D#FUNCSTART=%(line-1)
  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):16 %s -- %s | %FileCheck %s --check-prefix=EXPLICIT_CAPTURE_PARAM_REF -D#FUNCSTART=%(line-2)
  if let bar = bar {
    // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):11 %s -- %s | %FileCheck %s --check-prefix=EXPLICIT_CAPTURE_REF -D#FUNCSTART=%(line-4)
    print(bar)
  }
}

// EXPLICIT_CAPTURE: source.lang.swift.decl.var.local ([[#FUNCSTART+3]]:10-[[#FUNCSTART+3]]:13)
// EXPLICIT_CAPTURE: Int
// EXPLICIT_CAPTURE: SECONDARY SYMBOLS BEGIN
// EXPLICIT_CAPTURE-NEXT: SECONDARY SYMBOLS END

// EXPLICIT_CAPTURE_PARAM_REF: source.lang.swift.ref.var.local ([[#FUNCSTART]]:22-[[#FUNCSTART]]:25)
// EXPLICIT_CAPTURE_PARAM_REF: Int?
// EXPLICIT_CAPTURE_PARAM_REF: SECONDARY SYMBOLS BEGIN
// EXPLICIT_CAPTURE_PARAM_REF-NEXT: SECONDARY SYMBOLS END

// EXPLICIT_CAPTURE_REF: source.lang.swift.ref.var.local ([[#FUNCSTART+3]]:10-[[#FUNCSTART+3]]:13)
// EXPLICIT_CAPTURE_REF: Int
// EXPLICIT_CAPTURE_REF: SECONDARY SYMBOLS BEGIN
// EXPLICIT_CAPTURE_REF-NEXT: SECONDARY SYMBOLS END

func multipleShorthand(bar: Int?, baz: Int?) {
  // RUN: %sourcekitd-test -req=cursor -pos=%(line+2):10 %s -- %s | %FileCheck %s --check-prefix=MULTIPLE_SHORTHAND_BAR -D#FUNCSTART=%(line-1)
  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):19 %s -- %s | %FileCheck %s --check-prefix=MULTIPLE_SHORTHAND_BAZ -D#FUNCSTART=%(line-2)
  if let bar, let baz {
    // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):11 %s -- %s | %FileCheck %s --check-prefix=MULTIPLE_SHORTHAND_BAR_REF -D#FUNCSTART=%(line-4)
    print(bar)
    // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):11 %s -- %s | %FileCheck %s --check-prefix=MULTIPLE_SHORTHAND_BAZ_REF -D#FUNCSTART=%(line-6)
    print(baz)
  }
}

// MULTIPLE_SHORTHAND_BAR: source.lang.swift.decl.var.local ([[#FUNCSTART+3]]:10-[[#FUNCSTART+3]]:13)
// MULTIPLE_SHORTHAND_BAR: Int
// MULTIPLE_SHORTHAND_BAR: SECONDARY SYMBOLS BEGIN
// MULTIPLE_SHORTHAND_BAR.lang.swift.ref.var.local ([[#FUNCSTART]]:24-[[#FUNCSTART]]:27)
// MULTIPLE_SHORTHAND_BAR: Int?

// MULTIPLE_SHORTHAND_BAZ: source.lang.swift.decl.var.local ([[#FUNCSTART+3]]:19-[[#FUNCSTART+3]]:22)
// MULTIPLE_SHORTHAND_BAZ: Int
// MULTIPLE_SHORTHAND_BAZ: SECONDARY SYMBOLS BEGIN
// MULTIPLE_SHORTHAND_BAZ.lang.swift.ref.var.local ([[#FUNCSTART]]:35-[[#FUNCSTART]]:38)
// MULTIPLE_SHORTHAND_BAZ: Int?

// MULTIPLE_SHORTHAND_BAR_REF: source.lang.swift.ref.var.local ([[#FUNCSTART+3]]:10-[[#FUNCSTART+3]]:13)
// MULTIPLE_SHORTHAND_BAR_REF: Int
// MULTIPLE_SHORTHAND_BAR_REF: SECONDARY SYMBOLS BEGIN
// MULTIPLE_SHORTHAND_BAR_REF-NEXT: SECONDARY SYMBOLS END

// MULTIPLE_SHORTHAND_BAZ_REF: source.lang.swift.ref.var.local ([[#FUNCSTART+3]]:19-[[#FUNCSTART+3]]:22)
// MULTIPLE_SHORTHAND_BAZ_REF: Int
// MULTIPLE_SHORTHAND_BAZ_REF: SECONDARY SYMBOLS BEGIN
// MULTIPLE_SHORTHAND_BAZ_REF-NEXT: SECONDARY SYMBOLS END

func guardLet(bar: Int?) {
  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):13 %s -- %s | %FileCheck %s --check-prefix=GUARD_LET -D#FUNCSTART=%(line-1)
  guard let bar else {
    return
  }
  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):9 %s -- %s | %FileCheck %s --check-prefix=GUARD_LET_REF -D#FUNCSTART=%(line-5)
  print(bar)
}

// GUARD_LET: source.lang.swift.decl.var.local ([[#FUNCSTART+2]]:13-[[#FUNCSTART+2]]:16)
// GUARD_LET: Int
// GUARD_LET: SECONDARY SYMBOLS BEGIN
// GUARD_LET: source.lang.swift.ref.var.local ([[#FUNCSTART]]:15-[[#FUNCSTART]]:18)
// GUARD_LET: Int?

// GUARD_LET_REF: source.lang.swift.ref.var.local ([[#FUNCSTART+2]]:13-[[#FUNCSTART+2]]:16)
// GUARD_LET_REF: Int
// GUARD_LET_REF: SECONDARY SYMBOLS BEGIN
// GUARD_LET_REF-NEXT: SECONDARY SYMBOLS END

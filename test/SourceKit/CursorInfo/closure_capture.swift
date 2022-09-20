func takeClosure(_ closure: () -> Void) {}

// RUN: %sourcekitd-test -req=cursor -pos=%(line+1):13 %s -- %s | %FileCheck %s --check-prefix=SIMPLE_PARAM -D#FUNCSTART=%(line+1)
func simple(bar: Int) {
  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):18 %s -- %s | %FileCheck %s --check-prefix=SIMPLE_CAPTURE -D#FUNCSTART=%(line-1)
  takeClosure { [bar] in
    // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):11 %s -- %s | %FileCheck %s --check-prefix=SIMPLE_REF -D#FUNCSTART=%(line-3)
    print(bar)
  }
}

// SIMPLE_PARAM: source.lang.swift.decl.var.parameter ([[#FUNCSTART]]:13-[[#FUNCSTART]]:16)
// SIMPLE_PARAM: SECONDARY SYMBOLS BEGIN
// There should be no secondary symbols
// SIMPLE_PARAM-NEXT: SECONDARY SYMBOLS END

// SIMPLE_CAPTURE: source.lang.swift.decl.var.local ([[#FUNCSTART+2]]:18-[[#FUNCSTART+2]]:21)
// SIMPLE_CAPTURE: SECONDARY SYMBOLS BEGIN
// SIMPLE_CAPTURE: source.lang.swift.ref.var.local ([[#FUNCSTART]]:13-[[#FUNCSTART]]:16)

// SIMPLE_REF: source.lang.swift.ref.var.local ([[#FUNCSTART+2]]:18-[[#FUNCSTART+2]]:21)
// SIMPLE_REF: SECONDARY SYMBOLS BEGIN
// SIMPLE_REF-NEXT: SECONDARY SYMBOLS END

func doubleNested(bar: Int) {
  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):18 %s -- %s | %FileCheck %s --check-prefix=DOUBLE_NESTED_FIRST_CAPTURE -D#FUNCSTART=%(line-1)
  takeClosure { [bar] in
    // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):20 %s -- %s | %FileCheck %s --check-prefix=DOUBLE_NESTED_SECOND_CAPTURE -D#FUNCSTART=%(line-3)
    takeClosure { [bar] in
      // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):13 %s -- %s | %FileCheck %s --check-prefix=DOUBLE_NESTED_REF -D#FUNCSTART=%(line-5)
      print(bar)
    }
  }
}

// DOUBLE_NESTED_FIRST_CAPTURE: source.lang.swift.decl.var.local ([[#FUNCSTART+2]]:18-[[#FUNCSTART+2]]:21)
// DOUBLE_NESTED_FIRST_CAPTURE: SECONDARY SYMBOLS BEGIN
// DOUBLE_NESTED_FIRST_CAPTURE: source.lang.swift.ref.var.local ([[#FUNCSTART]]:19-[[#FUNCSTART]]:22)

// DOUBLE_NESTED_SECOND_CAPTURE: source.lang.swift.decl.var.local ([[#FUNCSTART+4]]:20-[[#FUNCSTART+4]]:23)
// DOUBLE_NESTED_SECOND_CAPTURE: SECONDARY SYMBOLS BEGIN
// DOUBLE_NESTED_SECOND_CAPTURE: source.lang.swift.ref.var.local ([[#FUNCSTART+2]]:18-[[#FUNCSTART+2]]:21)
// DOUBLE_NESTED_SECOND_CAPTURE: source.lang.swift.ref.var.local ([[#FUNCSTART]]:19-[[#FUNCSTART]]:22)

// DOUBLE_NESTED_REF: source.lang.swift.ref.var.local ([[#FUNCSTART+4]]:20-[[#FUNCSTART+4]]:23)
// DOUBLE_NESTED_REF: SECONDARY SYMBOLS BEGIN
// DOUBLE_NESTED_REF-NEXT: SECONDARY SYMBOLS END

// Make sure we don't report secondary symbols if the variable is captured explicitly using '='
func explicitCapture(bar: Int) {
  // RUN: %sourcekitd-test -req=cursor -pos=%(line+2):18 %s -- %s | %FileCheck %s --check-prefix=EXPLICIT_CAPTURE -D#FUNCSTART=%(line-1)
  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):24 %s -- %s | %FileCheck %s --check-prefix=EXPLICIT_CAPTURE_PARAM_REF -D#FUNCSTART=%(line-2)
  takeClosure { [bar = bar] in
    // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):11 %s -- %s | %FileCheck %s --check-prefix=EXPLICIT_CAPTURE_REF -D#FUNCSTART=%(line-4)
    print(bar)
  }
}

// EXPLICIT_CAPTURE: source.lang.swift.decl.var.local ([[#FUNCSTART+3]]:18-[[#FUNCSTART+3]]:21)
// EXPLICIT_CAPTURE: SECONDARY SYMBOLS BEGIN
// EXPLICIT_CAPTURE-NEXT: SECONDARY SYMBOLS END

// EXPLICIT_CAPTURE_PARAM_REF: source.lang.swift.ref.var.local ([[#FUNCSTART]]:22-[[#FUNCSTART]]:25)
// EXPLICIT_CAPTURE_PARAM_REF: SECONDARY SYMBOLS BEGIN
// EXPLICIT_CAPTURE_PARAM_REF-NEXT: SECONDARY SYMBOLS END

// EXPLICIT_CAPTURE_REF: source.lang.swift.ref.var.local ([[#FUNCSTART+3]]:18-[[#FUNCSTART+3]]:21)
// EXPLICIT_CAPTURE_REF: SECONDARY SYMBOLS BEGIN
// There should be no secondary symbols
// EXPLICIT_CAPTURE_REF-NEXT: SECONDARY SYMBOLS END

func multipleCaptures(bar: Int, baz: Int) {
  // RUN: %sourcekitd-test -req=cursor -pos=%(line+2):18 %s -- %s | %FileCheck %s --check-prefix=MULTIPLE_CAPTURES_BAR -D#FUNCSTART=%(line-1)
  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):23 %s -- %s | %FileCheck %s --check-prefix=MULTIPLE_CAPTURES_BAZ -D#FUNCSTART=%(line-2)
  takeClosure { [bar, baz] in
    // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):11 %s -- %s | %FileCheck %s --check-prefix=MULTIPLE_CAPTURES_BAR_REF -D#FUNCSTART=%(line-4)
    print(bar)
    // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):11 %s -- %s | %FileCheck %s --check-prefix=MULTIPLE_CAPTURES_BAZ_REF -D#FUNCSTART=%(line-6)
    print(baz)
  }
}

// MULTIPLE_CAPTURES_BAR: source.lang.swift.decl.var.local ([[#FUNCSTART+3]]:18-[[#FUNCSTART+3]]:21)
// MULTIPLE_CAPTURES_BAR: SECONDARY SYMBOLS BEGIN
// MULTIPLE_CAPTURES_BAR: source.lang.swift.ref.var.local ([[#FUNCSTART]]:23-[[#FUNCSTART]]:26)

// MULTIPLE_CAPTURES_BAZ: source.lang.swift.decl.var.local ([[#FUNCSTART+3]]:23-[[#FUNCSTART+3]]:26)
// MULTIPLE_CAPTURES_BAZ: SECONDARY SYMBOLS BEGIN
// MULTIPLE_CAPTURES_BAZ: source.lang.swift.ref.var.local ([[#FUNCSTART]]:33-[[#FUNCSTART]]:36)

// MULTIPLE_CAPTURES_BAR_REF: source.lang.swift.ref.var.local ([[#FUNCSTART+3]]:18-[[#FUNCSTART+3]]:21)
// MULTIPLE_CAPTURES_BAR_REF: SECONDARY SYMBOLS BEGIN
// MULTIPLE_CAPTURES_BAR_REF-NEXT: SECONDARY SYMBOLS END

// MULTIPLE_CAPTURES_BAZ_REF: source.lang.swift.ref.var.local ([[#FUNCSTART+3]]:23-[[#FUNCSTART+3]]:26)
// MULTIPLE_CAPTURES_BAZ_REF: SECONDARY SYMBOLS BEGIN
// MULTIPLE_CAPTURES_BAZ_REF-NEXT: SECONDARY SYMBOLS END

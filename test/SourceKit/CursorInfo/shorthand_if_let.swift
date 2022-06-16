func simple(bar: Int?) {
  if let bar {
    print(bar)
  }
}

// RUN: %sourcekitd-test -req=cursor -pos=1:13 %s -- %s | %FileCheck %s --check-prefix=SIMPLE_PARAM
// SIMPLE_PARAM: source.lang.swift.decl.var.parameter (1:13-1:16)
// SIMPLE_PARAM: Int
// SIMPLE_PARAM: SECONDARY SYMBOLS BEGIN
// There should be no secondary symbols
// SIMPLE_PARAM-NEXT: SECONDARY SYMBOLS END

// RUN: %sourcekitd-test -req=cursor -pos=2:10 %s -- %s | %FileCheck %s --check-prefix=SIMPLE_CAPTURE
// SIMPLE_CAPTURE: source.lang.swift.decl.var.local (2:10-2:13)
// SIMPLE_CAPTURE: Int
// SIMPLE_CAPTURE: SECONDARY SYMBOLS BEGIN
// SIMPLE_CAPTURE: source.lang.swift.ref.var.local (1:13-1:16)
// SIMPLE_CAPTURE: Int?

// RUN: %sourcekitd-test -req=cursor -pos=3:11 %s -- %s | %FileCheck %s --check-prefix=SIMPLE_REF
// SIMPLE_REF: source.lang.swift.ref.var.local (2:10-2:13)
// SIMPLE_REF: Int
// SIMPLE_REF: SECONDARY SYMBOLS BEGIN
// SIMPLE_REF: source.lang.swift.ref.var.local (1:13-1:16)
// SIMPLE_REF: Int?

func doubleNested(bar: Int??) {
  if let bar {
    if let bar {
      print(bar)
    }
  }
}

// RUN: %sourcekitd-test -req=cursor -pos=29:10 %s -- %s | %FileCheck %s --check-prefix=DOUBLE_NESTED_FIRST_CAPTURE
// DOUBLE_NESTED_FIRST_CAPTURE: source.lang.swift.decl.var.local (29:10-29:13)
// DOUBLE_NESTED_FIRST_CAPTURE: Int?
// DOUBLE_NESTED_FIRST_CAPTURE: SECONDARY SYMBOLS BEGIN
// DOUBLE_NESTED_FIRST_CAPTURE: source.lang.swift.ref.var.local (28:19-28:22)
// DOUBLE_NESTED_FIRST_CAPTURE: Int??

// RUN: %sourcekitd-test -req=cursor -pos=30:12 %s -- %s | %FileCheck %s --check-prefix=DOUBLE_NESTED_SECOND_CAPTURE
// DOUBLE_NESTED_SECOND_CAPTURE: source.lang.swift.decl.var.local (30:12-30:15)
// DOUBLE_NESTED_SECOND_CAPTURE: Int
// DOUBLE_NESTED_SECOND_CAPTURE: SECONDARY SYMBOLS BEGIN
// DOUBLE_NESTED_SECOND_CAPTURE: source.lang.swift.ref.var.local (29:10-29:13)
// DOUBLE_NESTED_SECOND_CAPTURE: Int?
// DOUBLE_NESTED_SECOND_CAPTURE: source.lang.swift.ref.var.local (28:19-28:22)
// DOUBLE_NESTED_SECOND_CAPTURE: Int??

// RUN: %sourcekitd-test -req=cursor -pos=31:13 %s -- %s | %FileCheck %s --check-prefix=DOUBLE_NESTED_REF
// DOUBLE_NESTED_REF: source.lang.swift.ref.var.local (30:12-30:15)
// DOUBLE_NESTED_REF: Int
// DOUBLE_NESTED_REF: SECONDARY SYMBOLS BEGIN
// DOUBLE_NESTED_REF: source.lang.swift.ref.var.local (29:10-29:13)
// DOUBLE_NESTED_REF: Int?
// DOUBLE_NESTED_REF: source.lang.swift.ref.var.local (28:19-28:22)
// DOUBLE_NESTED_REF: Int??

// Make sure we don't report secondary symbols if the variable is captured explicitly using '='
func explicitCapture(bar: Int?) {
  if let bar = bar {
    print(bar)
  }
}

// RUN: %sourcekitd-test -req=cursor -pos=64:11 %s -- %s | %FileCheck %s --check-prefix=EXPLICIT_CAPTURE_REF
// EXPLICIT_CAPTURE_REF: source.lang.swift.ref.var.local (63:10-63:13)
// EXPLICIT_CAPTURE_REF: Int
// EXPLICIT_CAPTURE_REF: SECONDARY SYMBOLS BEGIN
// There should be no secondary symbols
// EXPLICIT_CAPTURE_REF-NEXT: SECONDARY SYMBOLS END

func multipleShorthand(bar: Int?, baz: Int?) {
  if let bar, let baz {
    print(bar)
    print(baz)
  }
}

// RUN: %sourcekitd-test -req=cursor -pos=77:11 %s -- %s | %FileCheck %s --check-prefix=MULTIPLE_SHORTHAND_BAR
// MULTIPLE_SHORTHAND_BAR: source.lang.swift.ref.var.local (76:10-76:13)
// MULTIPLE_SHORTHAND_BAR: Int
// MULTIPLE_SHORTHAND_BAR: SECONDARY SYMBOLS BEGIN
// MULTIPLE_SHORTHAND_BAR.lang.swift.ref.var.local (75:23-75:26)
// MULTIPLE_SHORTHAND_BAR: Int?

// RUN: %sourcekitd-test -req=cursor -pos=78:11 %s -- %s | %FileCheck %s --check-prefix=MULTIPLE_SHORTHAND_BAZ
// MULTIPLE_SHORTHAND_BAZ: source.lang.swift.ref.var.local (76:19-76:22)
// MULTIPLE_SHORTHAND_BAZ: Int
// MULTIPLE_SHORTHAND_BAZ: SECONDARY SYMBOLS BEGIN
// MULTIPLE_SHORTHAND_BAZ.lang.swift.ref.var.local (63:33-63:36)
// MULTIPLE_SHORTHAND_BAZ: Int?

func guardLet(bar: Int?) {
  guard let bar else {
    return
  }
  print(bar)
}

// RUN: %sourcekitd-test -req=cursor -pos=100:9 %s -- %s | %FileCheck %s --check-prefix=GUARD_LET
// GUARD_LET: source.lang.swift.ref.var.local (97:13-97:16)
// GUARD_LET: Int
// GUARD_LET: SECONDARY SYMBOLS BEGIN
// GUARD_LET.lang.swift.ref.var.local (96:15-96:18)
// GUARD_LET: Int?

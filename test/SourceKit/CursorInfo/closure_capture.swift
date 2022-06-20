func takeClosure(_ closure: () -> Void) {}

func simple(bar: Int) {
  takeClosure { [bar] in
    print(bar)
  }
}

// RUN: %sourcekitd-test -req=cursor -pos=3:13 %s -- %s | %FileCheck %s --check-prefix=SIMPLE_PARAM
// SIMPLE_PARAM: source.lang.swift.decl.var.parameter (3:13-3:16)
// SIMPLE_PARAM: SECONDARY SYMBOLS BEGIN
// There should be no secondary symbols
// SIMPLE_PARAM-NEXT: SECONDARY SYMBOLS END

// RUN: %sourcekitd-test -req=cursor -pos=4:18 %s -- %s | %FileCheck %s --check-prefix=SIMPLE_CAPTURE
// SIMPLE_CAPTURE: source.lang.swift.decl.var.local (4:18-4:21)
// SIMPLE_CAPTURE: SECONDARY SYMBOLS BEGIN
// SIMPLE_CAPTURE: source.lang.swift.ref.var.local (3:13-3:16)

// RUN: %sourcekitd-test -req=cursor -pos=5:11 %s -- %s | %FileCheck %s --check-prefix=SIMPLE_REF
// SIMPLE_REF: source.lang.swift.ref.var.local (4:18-4:21)
// SIMPLE_REF: SECONDARY SYMBOLS BEGIN
// SIMPLE_REF: source.lang.swift.ref.var.local (3:13-3:16)

func doubleNested(bar: Int) {
  takeClosure { [bar] in
    takeClosure { [bar] in
      print(bar)
    }
  }
}

// RUN: %sourcekitd-test -req=cursor -pos=26:18 %s -- %s | %FileCheck %s --check-prefix=DOUBLE_NESTED_FIRST_CAPTURE
// DOUBLE_NESTED_FIRST_CAPTURE: source.lang.swift.decl.var.local (26:18-26:21)
// DOUBLE_NESTED_FIRST_CAPTURE: SECONDARY SYMBOLS BEGIN
// DOUBLE_NESTED_FIRST_CAPTURE: source.lang.swift.ref.var.local (25:19-25:22)

// RUN: %sourcekitd-test -req=cursor -pos=27:20 %s -- %s | %FileCheck %s --check-prefix=DOUBLE_NESTED_SECOND_CAPTURE
// DOUBLE_NESTED_SECOND_CAPTURE: source.lang.swift.decl.var.local (27:20-27:23)
// DOUBLE_NESTED_SECOND_CAPTURE: SECONDARY SYMBOLS BEGIN
// DOUBLE_NESTED_SECOND_CAPTURE: source.lang.swift.ref.var.local (26:18-26:21)
// DOUBLE_NESTED_SECOND_CAPTURE: source.lang.swift.ref.var.local (25:19-25:22)

// RUN: %sourcekitd-test -req=cursor -pos=28:13 %s -- %s | %FileCheck %s --check-prefix=DOUBLE_NESTED_REF
// DOUBLE_NESTED_REF: source.lang.swift.ref.var.local (27:20-27:23)
// DOUBLE_NESTED_REF: SECONDARY SYMBOLS BEGIN
// DOUBLE_NESTED_REF: source.lang.swift.ref.var.local (26:18-26:21)
// DOUBLE_NESTED_REF: source.lang.swift.ref.var.local (25:19-25:22)

// Make sure we don't report secondary symbols if the variable is captured explicitly using '='
func explicitCapture(bar: Int) {
  takeClosure { [bar = bar] in
    print(bar)
  }
}

// RUN: %sourcekitd-test -req=cursor -pos=53:11 %s -- %s | %FileCheck %s --check-prefix=EXPLICIT_CAPTURE_REF
// EXPLICIT_CAPTURE_REF: source.lang.swift.ref.var.local (52:18-52:21)
// EXPLICIT_CAPTURE_REF: SECONDARY SYMBOLS BEGIN
// There should be no secondary symbols
// EXPLICIT_CAPTURE_REF-NEXT: SECONDARY SYMBOLS END

func multipleCaptures(bar: Int, baz: Int) {
  takeClosure { [bar, baz] in
    print(bar)
    print(baz)
  }
}

// RUN: %sourcekitd-test -req=cursor -pos=65:11 %s -- %s | %FileCheck %s --check-prefix=MULTIPLE_CAPTURES_BAR
// MULTIPLE_CAPTURES_BAR: source.lang.swift.ref.var.local (64:18-64:21)
// MULTIPLE_CAPTURES_BAR: SECONDARY SYMBOLS BEGIN
// MULTIPLE_CAPTURES_BAR.lang.swift.ref.var.local (63:23-63:26)

// RUN: %sourcekitd-test -req=cursor -pos=66:11 %s -- %s | %FileCheck %s --check-prefix=MULTIPLE_CAPTURES_BAZ
// MULTIPLE_CAPTURES_BAZ: source.lang.swift.ref.var.local (64:23-64:26)
// MULTIPLE_CAPTURES_BAZ: SECONDARY SYMBOLS BEGIN
// MULTIPLE_CAPTURES_BAZ.lang.swift.ref.var.local (63:33-63:36)

func /*defaults:def*/withDefaults(_ xx: Int = 4, y: Int = 2, x: Int = 1) {}

// valid
/*defaults:call*/withDefaults()
/*defaults:call*/withDefaults(2)
/*defaults:call*/withDefaults(y: 2)
/*defaults:call*/withDefaults(2, x: 3)
/*defaults:call*/withDefaults(y: 2, x: 3)
/*defaults:call*/withDefaults(2, y: 1, x: 4)

// false positives
/*defaults:call*/withDefaults(y: 2, 3)
/*defaults:call*/withDefaults(y: 2, 4, x: 3)

// invalid
/*defaults:call*/withDefaults(x: 2, y: 3)


func /*trailing:def*/withTrailingClosure(x: Int, y: () -> Int) {}

// valid
/*trailing:call*/withTrailingClosure(x: 2, y: { return 1})
/*trailing:call*/withTrailingClosure(x: 2) { return 1}
/*trailing:call*/withTrailingClosure(x: 2)
{ return 1}
// invalid
/*trailing:call*/withTrailingClosure(x: 1, y: 2) { return 1}
/*trailing:call*/withTrailingClosure(x: 1, y: 2) { return 1}

func /*trailing-only:def*/trailingOnly(a: () -> ()) {}
/*trailing-only:call*/trailingOnly(a: {})
/*trailing-only:call*/trailingOnly {}


func /*varargs:def*/withVarargs(x: Int..., y: Int, _: Int) {}

// valid
/*varargs:call*/withVarargs(x: 1, 2, 3, y: 2, 4)
/*varargs:call*/withVarargs(y: 2, 4)

// false positives
/*varargs:call*/withVarargs(x: 1, y: 2, 4, 5)

//invalid
/*varargs:call*/withVarargs(2, y: 2)


func /*varargs2:def*/withVarargs(x: Int, y: Int, _: Int...) {}

// valid
/*varargs2:call*/withVarargs(x: 1, y: 2, 4, 5)
/*varargs2:call*/withVarargs(x: 1, y: 2)

// false positive
/*varargs2:call*/withVarargs(x: 1, 2, y: 2, 4)


func /*mixed:def*/withAllOfTheAbove(x: Int = 2, _: Int..., z: Int = 2, c: () -> Int) {}

// valid
/*mixed:call*/withAllOfTheAbove(2){ return 1 }
/*mixed:call*/withAllOfTheAbove(x: 1, 2, c: {return 1})
/*mixed:call*/withAllOfTheAbove(x: 1, c: {return 1})
/*mixed:call*/withAllOfTheAbove(1, z: 1) { return 1 }
/*mixed:call*/withAllOfTheAbove(1, 2, c: {return 1})

// false positives
/*mixed:call*/withAllOfTheAbove(z: 1, 2, c: {return 1})

// REQUIRES: swift_swift_parser
// RUN: %empty-directory(%t.result)
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="defaults" -is-function-like -old-name "withDefaults(_:y:x:)" -new-name "betterName(x:y:z:)" >> %t.result/callsites_defaults.swift
// RUN: diff -u %S/Outputs/callsites/defaults.swift.expected %t.result/callsites_defaults.swift
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="trailing" -is-function-like -old-name "withTrailingClosure(x:y:)" -new-name "betterName(a:b:)" >> %t.result/callsites_trailing.swift
// RUN: diff -u %S/Outputs/callsites/trailing.swift.expected %t.result/callsites_trailing.swift
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="trailing-only" -is-function-like -old-name "trailingOnly(a:)" -new-name "betterName(b:)" >> %t.result/callsites_trailing_only.swift
// RUN: diff -u %S/Outputs/callsites/trailing_only.swift.expected %t.result/callsites_trailing_only.swift
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="varargs" -is-function-like -old-name "withVarargs(x:y:_:)" -new-name "betterName(a:b:c:)" >> %t.result/callsites_varargs.swift
// RUN: diff -u %S/Outputs/callsites/varargs.swift.expected %t.result/callsites_varargs.swift
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="varargs2" -is-function-like -old-name "withVarargs(x:y:_:)" -new-name "betterName(a:b:c:)" >> %t.result/callsites_varargs2.swift
// RUN: diff -u %S/Outputs/callsites/varargs2.swift.expected %t.result/callsites_varargs2.swift
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="mixed" -is-function-like -old-name "withAllOfTheAbove(x:_:z:c:)" -new-name "betterName(a:b:c:d:)" >> %t.result/callsites_mixed.swift
// RUN: diff -u %S/Outputs/callsites/mixed.swift.expected %t.result/callsites_mixed.swift
// RUN: %empty-directory(%t.ranges)
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="defaults" -is-function-like -old-name "withDefaults(_:y:x:)" >> %t.ranges/callsites_defaults.swift
// RUN: diff -u %S/Outputs/callsites/defaults.swift.expected %t.ranges/callsites_defaults.swift
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="trailing" -is-function-like -old-name "withTrailingClosure(x:y:)" >> %t.ranges/callsites_trailing.swift
// RUN: diff -u %S/Outputs/callsites/trailing.swift.expected %t.ranges/callsites_trailing.swift
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="trailing-only" -is-function-like -old-name "trailingOnly(a:)" >> %t.ranges/callsites_trailing_only.swift
// RUN: diff -u %S/Outputs/callsites/trailing_only.swift.expected %t.ranges/callsites_trailing_only.swift

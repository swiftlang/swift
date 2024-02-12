// RUN: not %target-swift-frontend -diagnostic-style=swift -typecheck %/s 2>&1 | %FileCheck %s

// REQUIRES: swift_swift_parser
// REQUIRES: presumedlocbusted

// Error split between the real file and a virtual one.
#sourceLocation(file: "abc.swift", line: 9)
let x = 1
#sourceLocation()
let x = 2

// Error split between two virtual files.
#sourceLocation(file: "abc.swift", line: 4)
let y = 1
#sourceLocation(file: "xyz.swift", line: 18)
let y = 2
#sourceLocation()

// Error within a virtual file on non-consecutive lines.
#sourceLocation(file: "abc.swift", line: 1)
let z = 1
// space
let z = 2
#sourceLocation()

// Error with note location placed in the same virtual file via a separate #sourceLocation block.
#sourceLocation(file: "abc.swift", line: 1)
let a = 1
#sourceLocation()


#sourceLocation(file: "abc.swift", line: 10)
let a = 2
#sourceLocation()

// Error at the beginning of a virtual file.
#sourceLocation(file: "abc.swift", line: 1)
let any: Any = ""
let zz: Int = any
#sourceLocation()

// CHECK: SOURCE_DIR{{[/\]+}}test{{[/\]+}}diagnostics{{[/\]+}}pretty-printed-source-loc-directive-diags.swift:[[#LINE:]]:5
// CHECK: [[#LINE-1]] │ #sourceLocation()
// CHECK: [[#LINE]]   │ let x = 2
// CHECK:             │     ╰─ error: invalid redeclaration of 'x'
// CHECK: [[#LINE+1]] │
// CHECK: abc.swift:9:5
// CHECK: 9 │ let x = 1
// CHECK:   │     ╰─ note: 'x' previously declared here


// CHECK: abc.swift:4:5
// CHECK:  4 │ let y = 1
// CHECK:    │     ╰─ note: 'y' previously declared here
// CHECK: xyz.swift:18:5
// CHECK: 18 │ let y = 2
// CHECK:    │     ╰─ error: invalid redeclaration of 'y'


// CHECK: abc.swift:3:5
// CHECK:  1 │ let z = 1
// CHECK:    │     ╰─ note: 'z' previously declared here
// CHECK:   ...
// CHECK:  3 │ let z = 2
// CHECK:    │     ╰─ error: invalid redeclaration of 'z'


// CHECK: abc.swift:10:5
// CHECK:  1 │ let a = 1
// CHECK:    │     ╰─ note: 'a' previously declared here
// CHECK:   ...
// CHECK: 10 │ let a = 2
// CHECK:    │     ╰─ error: invalid redeclaration of 'a'


// CHECK: abc.swift:2:15
// CHECK:  2 │ let zz: Int = any as! Int
// CHECK:    │               ╰─ error: cannot convert value of type 'Any' to specified type 'Int' [insert ' as! Int']


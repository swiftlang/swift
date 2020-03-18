let foo = Bar.Stuff(
    first: path,
    description: "No \(thing) was found at path \(path)"

// RUN: %sourcekitd-test -req=format -line=4 -length=1 %s | %FileCheck --strict-whitespace %s
// CHECK: key.sourcetext: ""

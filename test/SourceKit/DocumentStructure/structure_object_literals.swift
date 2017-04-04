// RUN: %sourcekitd-test -req=structure %s -- -module-name StructureTest %s | %sed_clean > %t.response
// RUN: diff -u %s.response %t.response

let color: S = #colorLiteral(red: 1, green: 0, blue: 0, alpha: 1)
let image: I? = #imageLiteral(resourceName: "hello.png")

// XFAIL: linux

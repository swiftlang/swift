// RUN: %sourcekitd-test -req=structure %s -- -module-name StructureTest %s | %sed_clean > %t.response
// RUN: diff -u %s.response %t.response

let color: S = [#Color(colorLiteralRed: 1, green: 0, blue: 0, alpha: 1)#]
let image: I? = [#Image(imageLiteral: "hello.png")#]

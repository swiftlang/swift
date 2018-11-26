// RUN: %sourcekitd-test -req=syntax-map %s | %sed_clean > %t.response
// RUN: diff -u %s.response %t.response

let image = #imageLiteral(resourceName: "cloud.png")
let color = #colorLiteral(red: 1, blue: 0, green: 1, alpha: 1)
let file = #fileLiteral(resourceName: "test.txt")


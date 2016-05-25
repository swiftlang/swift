// RUN: %sourcekitd-test -req=syntax-map %s | %sed_clean > %t.response
// RUN: diff -u %s.response %t.response

let fn = #function
let f = #file
let l = #line
let c = #column

if #available(iOS 9.0, *) {}


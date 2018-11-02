// RUN: %sourcekitd-test -req=syntax-map %s | %sed_clean > %t.response
// RUN: diff -u %s.response %t.response
// RUN: %sourcekitd-test -req=syntax-map %s -force-libsyntax-based-processing | %sed_clean > %t.libsyntax.response
// RUN: diff -u %s.libsyntax.response %t.libsyntax.response

let fn = #function
let f = #file
let l = #line
let c = #column

if #available(iOS 9.0, *) {}

#if false
#error("Error")
#elseif true
#warning("Warning")
#else
#sourceLocation(file: "here.swift", line:100)
#sourceLocation()
#endif

// RUN: %sourcekitd-test -req=syntax-map %s > %t.response
// RUN: diff -u %s.response %t.response

let x = [#Cloud(tube: true)#]

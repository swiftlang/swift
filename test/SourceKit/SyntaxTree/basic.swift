// RUN: %swift-syntax-test -input-source-filename %s -serialize-raw-tree > %t.emit
// RUN: %sourcekitd-test -req=syntax-tree %s > %t.sourcekit
// RUN: diff %t.emit %t.sourcekit

struct Foo {
  let   bar : Int

  let baz : Array < Int >
      }

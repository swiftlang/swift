struct Foo {
  func bar(body: Invalid) {}

// RUN: %sourcekitd-test -req=related-idents -pos=%(line + 1):8 %s -- %s
  func bar(ignoreCase: Bool, body: Invalid) {}
}

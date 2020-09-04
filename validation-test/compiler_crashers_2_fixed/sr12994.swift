// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -index-store-path %t/idx -o %t/file.o -typecheck -primary-file %s -verify

protocol MyProto {
  func compile() throws
}

func compile(x: MyProto) throws {
  try x.compile 
  // expected-error@-1 {{expression resolves to an unused function}}
  // expected-warning@-2 {{no calls to throwing functions occur within 'try' expression}}
}

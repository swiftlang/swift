// RUN: %target-swift-frontend -parse -verify %s

func test1() {
  @s // expected-astgen-error {{expected declaration after attribute}}
  return // expected-cparser-error {{expected declaration}}
}
func test2() {
  @unknown // expected-error {{unknown attribute 'unknown'}} expected-astgen-error {{expected declaration after attribute}}
  return // expected-cparser-error {{expected declaration}}
}

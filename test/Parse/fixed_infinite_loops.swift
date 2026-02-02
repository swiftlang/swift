// RUN: %target-swift-frontend -parse -verify %s

func test1() {
  @s
  return // expected-error {{expected declaration}}
}
func test2() {
  @unknown // expected-error {{unknown attribute 'unknown'}}
  return // expected-error {{expected declaration}}
}

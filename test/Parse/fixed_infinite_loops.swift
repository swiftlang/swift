// RUN: %target-swift-frontend -parse -verify %s

func test1() {
  @s // expected-error {{expected statement}}
  return
}
func test2() {
  @unknown // expected-error {{expected statement}}
  return
}

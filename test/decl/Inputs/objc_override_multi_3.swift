class Super1 {
  @objc(a)
  func f() { } // expected-note{{Objective-C method 'a' defined by method 'f()' here}}
}

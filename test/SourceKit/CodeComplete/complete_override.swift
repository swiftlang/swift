class Base {
  func f(getMe a: Int, b: Double) {}
}
class Derived : Base {

}

// RUN: %sourcekitd-test -req=complete -pos=5:1 %s -- %s > %t.response
// RUN: %diff -u %s.response %t.response


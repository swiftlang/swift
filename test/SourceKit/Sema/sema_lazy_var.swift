var gx = 0

class C1 {
  lazy var lazy_bar : Int = {
    return gx
  }()
}

// RUN: %sourcekitd-test -req=sema %s -- %s > %t.response
// RUN: %diff -u %s.response %t.response

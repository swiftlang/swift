// RUN: %target-swift-frontend -c -O %s

protocol P {}
enum E {
  case c(any P)
  var d: [String:String] { [:] }
}

final class C {
  var o: [String:String]?
}

func f(_ e: E?) {
  if let e {
    C().o?.merge(e.d) { c, _ in c }
  }
}

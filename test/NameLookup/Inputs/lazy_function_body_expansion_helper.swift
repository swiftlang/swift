// Body of closure in parameter to call of closureTaker is created lazily
// and this test ensures that that body scope does get expanded
var v = closureTaker {
  func amIFound() {}
}

func closureTaker(_: () -> Void )

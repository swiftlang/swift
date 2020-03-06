// RUN: not %target-swift-frontend %s -typecheck

struct SD {
  var array: [Int] = []
}

func test(sd: SD) {
  _ = sd[\.array[@]]
}

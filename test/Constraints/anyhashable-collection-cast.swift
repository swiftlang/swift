// RUN: %target-swift-frontend -parse -verify %s

func dict() -> [AnyHashable: Any] {
   return ["x": "y"]
}
func set() -> Set<AnyHashable> {
   return ["x"]
}

func test() {
   if let d = dict() as? [String: String] {
    print(d)
   }
   if let s = set() as? Set<String> {
    print(s)
   }
}

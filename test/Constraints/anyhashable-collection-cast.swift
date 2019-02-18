// RUN: %target-typecheck-verify-swift

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

func testLValueCoerce() {
	var lvalue = "lvalue"
	var map: [AnyHashable : Any] = [lvalue: lvalue]
	lvalue = map[lvalue] as! String
}

// RUN: %target-typecheck-verify-swift

func test1(_: ()) {}
test1(())
func test2() {}
test2()

func test4(_: (Int, Int) -> ()) {}
test4({ (x,y) in })
func test5(_: (Int, Int, Int) -> ()) {}
test5({ (x,y,z) in })

func test6(_: ((Int, Int)) -> ()) {}
test6({ (x,y) in })
func test7(_: ((Int, Int, Int)) -> ()) {}
test7({ (x,y,z) in })
test6({ (_ x, _ y) in })
test6({ (_, _) in })
test6({ (x:Int, y:Int) in })
test6({ (_, _) ->() in })

func test8(_: ((Int, Int)) -> Int) {}
test8 { (_, _) -> Int in 2 }
test8 { (x, y) in x }

let items = Array(zip(0..<10, 0..<10))
_ = items.filter { (_, x) in x.isMultiple(of: 2) }
_ = items.filter { _ in true }

func toString(indexes: Int?...) -> String {
  let _ = indexes.enumerated().map({ (i: Int, index: Int?) -> String? in
    let _: Int = i
    if index != nil {}
    return ""
  })
  let _ = [(1, 2)].contains { $0 != $1 }
  _ = ["Hello" : 2].map { ($0, ($1)) }
}

extension Dictionary {
  public mutating func merge(with dictionary: Dictionary) {
    dictionary.forEach { updateValue($1, forKey: $0) }
  }
}

let dictionary: [String: String] = [:]
_ = dictionary.first { (column, value) in true }!.value

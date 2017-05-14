// RUN: %target-swift-frontend -typecheck %s -swift-version 3
// RUN: %target-swift-frontend -typecheck -update-code -primary-file %s -emit-migrated-file-path %t.result -disable-migrator-fixits -swift-version 3
// RUN: diff -u %s.expected %t.result
// RUN: %target-swift-frontend -typecheck %s.expected -swift-version 4
// rdar://
// XFAIL: *

func test1(_: ()) {}
test1(())
test1()
func test2() {}
test2()

enum Result<T> {
	case success(T)
}
func test3(_: Result<()>) {}
test3(.success())

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

func toString(indexes: Int?...) -> String {
  let _ = indexes.enumerated().flatMap({ (i: Int, index: Int?) -> String? in
    let _: Int = i
    if index != nil {}
    return ""
  })
}

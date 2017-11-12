// RUN: not %target-swift-frontend -typecheck -primary-file %s

class IndexPath {
  let item: Int
  let row: Int

  init(item: Int, section: Int) {
    self.item = item
    self.row = section
  }
}

// Not valid code of course ("[Element]" would need to be "[String]")
// but compiler shouldn't crash
extension Array where Element == (String, [Element]) {

    subscript(_ indexPath: IndexPath) -> String {
          return self[indexPath.section].1[indexPath.row]
    }

}

let collection = [("foo", ["first"])]

let indexPath = IndexPath(item: 0, section: 0)
let e = collection[indexPath]


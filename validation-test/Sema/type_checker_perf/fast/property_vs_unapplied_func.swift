// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asan

struct Date {
  var description: String
  func description(with: Int) -> String { "" }
}

struct DatabaseLog {
  let string: String
  let values: [String]
  let date: Date

  var description: String {
    return "[" + string + "] [" + date.description + "] " + string + " [" + values.joined(separator: ", ") + "]"
  }
}


func rdar47742750(arr1: [Int], arr2: [Int]?) {
  let _ = {
    assert(arr1.count == arr1.count + (arr2 == nil ? 0 : 1 + arr2!.count + arr2!.count + arr2!.count))
  }
}

func f(
  a: String?,
  b: String?,
  c: Int,
  d: String?,
  e: String?
) -> Int {
  return (a?.unicodeScalars.count ?? 0) +
  (b?.unicodeScalars.count ?? 0) +
  c +
  (d?.unicodeScalars.count ?? 0) +
  (e?.unicodeScalars.count ?? 0)
}

// Dictionary compact map values benchmark
import TestsUtils

public let DictionaryCompactMapValues = [
  BenchmarkInfo(name: "DictionaryCompactMapValuesOfNilValue", runFunction: run_DictionaryCompactMapValuesOfNilValue, tags: [.validation, .api, .Dictionary]),
  BenchmarkInfo(name: "DictionaryCompactMapValuesOfCastValue", runFunction: run_DictionaryCompactMapValuesOfCastValue, tags: [.validation, .api, .Dictionary]),
]

@inline(never)
public func run_DictionaryCompactMapValuesOfNilValue(_ N: Int) {
  let size = 100
  var dict = [Int: Int?](minimumCapacity: size)

  // Fill Dictionary
  for i in 1...size {
    if i % 2 == 0 {
      dict[i] = nil
    } else {
      dict[i] = i
    }
  }
  CheckResults(dict.count == size / 2)

  var refDict = [Int: Int]()
  for i in stride(from: 1, to: 100, by: 2) {
    refDict[i] = i
  }

  var newDict = [Int: Int]()
  for _ in 1...1000*N {
    newDict = dict.compactMapValues({$0})
    if newDict != refDict {
      break
    }
  }

  CheckResults(newDict == refDict)
}

@inline(never)
public func run_DictionaryCompactMapValuesOfCastValue(_ N: Int) {
  let size = 100
  var dict = [Int: String](minimumCapacity: size)
  
  // Fill Dictionary
  for i in 1...size {
    if i % 2 == 0 {
      dict[i] = "dummy"
    } else {
      dict[i] = "\(i)"
    }
  }
  
  CheckResults(dict.count == size)

  var refDict = [Int: Int]()
  for i in stride(from: 1, to: 100, by: 2) {
    refDict[i] = i
  }

  var newDict = [Int: Int]()
  for _ in 1...1000*N {
    newDict = dict.compactMapValues(Int.init)
    if newDict != refDict {
      break
    }
  }
  
  CheckResults(newDict == refDict)
}


// 02_collections.swift - Collection types and operations

// ========== COLLECTION TYPES ==========
// Arrays
let emptyArray: [Int] = []
let arrayLiteral: [Int] = [1, 2, 3, 4, 5]
let repeatedArray = Array(repeating: 0, count: 5)
var mutableArray = [1, 2, 3]
mutableArray.append(4)
mutableArray.insert(0, at: 0)
mutableArray.remove(at: 1)
mutableArray.removeLast()
mutableArray.removeAll()

// Sets
let emptySet: Set<Int> = []
let setLiteral: Set<Int> = [1, 2, 3, 4, 5]
var mutableSet: Set<Int> = [1, 2, 3]
mutableSet.insert(4)
mutableSet.remove(2)
_ = mutableSet.contains(1)

// Dictionaries
let emptyDict: [String: Int] = [:]
let dictLiteral: [String: Int] = ["a": 1, "b": 2, "c": 3]
var mutableDict: [String: Int] = ["x": 10]
mutableDict["y"] = 20
mutableDict["x"] = nil
mutableDict.removeValue(forKey: "y")

// Ranges
let closedRange = 1...5
let halfOpenRange = 1..<5
let oneSidedRange = ...5
let oneSidedRange2 = 5...

// Collection algorithms
extension Array where Element: Comparable {
    func sorted() -> [Element] {
        return self.sorted(by: <)
    }
    
    func binarySearch(for element: Element) -> Int? {
        var left = 0
        var right = count - 1
        
        while left <= right {
            let mid = (left + right) / 2
            if self[mid] == element {
                return mid
            } else if self[mid] < element {
                left = mid + 1
            } else {
                right = mid - 1
            }
        }
        return nil
    }
}

// Lazy collections
let lazyNumbers = (1...1000).lazy.filter { $0 % 2 == 0 }.map { $0 * $0 }

print("=== Collections ===")
print("Array: \(arrayLiteral)")
print("Set: \(setLiteral)")
print("Dictionary: \(dictLiteral)")
print("Range: \(closedRange)")
print("Sorted array: \(arrayLiteral.sorted())")
print("Binary search for 3: \(arrayLiteral.binarySearch(for: 3) ?? -1)")

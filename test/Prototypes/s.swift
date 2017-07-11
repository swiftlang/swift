// RUN: %target-run-simple-swift -O

import Dispatch
import Darwin

let args = CommandLine.arguments.dropFirst()
let testOld = !args.contains { $0.hasPrefix("--") } || args.contains("--old")
let testNew = !args.contains { $0.hasPrefix("--") } || args.contains("--new")
let testFilter = args.filter { !$0.hasPrefix("--") }

func timer(_ _caller : String = #function, _ body: ()->()) {
  if !testOld && _caller.hasSuffix("_old()") { return }
  if !testNew && _caller.hasSuffix("_new()") { return }
  
  if !testFilter.isEmpty
  && !testFilter.contains(where: { _caller.hasPrefix($0) }) { return  }
  
  var tmin = Double.infinity
  var tmax = 0.0, sum = 0.0
  var reps = testFilter.isEmpty ? 3 : 5
  _sanityCheck({ reps=1; return true }())
  
  for _ in 0..<reps {
    let start = DispatchTime.now()
    body()
    let end = DispatchTime.now()
    let milliseconds = (Double(end.uptimeNanoseconds) - Double(start.uptimeNanoseconds)) / 1_000_000.0
    tmin = min(tmin, milliseconds)
    sum += milliseconds
    tmax = max(tmax, milliseconds)
  }
  var prefix = "\(_caller),\(String(repeating: " ", count: 50 - _caller.utf16.count))\(Int(tmin)),"
  let spaces = String(repeating: " ", count: 60 - prefix.utf16.count)
  print(prefix, spaces, "±\(Int((sum / Double(reps) - tmin) / 2))")
}

public // @testable
func test_newStringCore(
  instrumentedWith time_: @escaping (String, ()->Void)->Void
) {
  /*
  let cat = _Concat3(5..<10, 15...20, (25...30).dropFirst())
  assert(cat.elementsEqual(cat.indices.map { cat[$0] }), "cat failure")
  */
  
  assert(MemoryLayout<String._Testable.UTF16View>.size <= 16)
  
  func time(_ _caller : String = #function, body: ()->()) {
    time_(_caller, body)
  }
  
  let testers: [String] = [
    "foo", "foobar", "foobarbaz", "foobarbazniz", "foobarbaznizman", "the quick brown fox",
    "f\u{f6}o", "f\u{f6}obar", "f\u{f6}obarbaz", "f\u{f6}obarbazniz", "f\u{f6}obarbaznizman", "the quick br\u{f6}wn fox",
    "ƒoo", "ƒoobar", "ƒoobarba", "ƒoobarbazniz", "ƒoobarbaznizman", "the quick brown ƒox"
  ]

  let cores
  = testers.map { $0._core } + testers.map { ($0 + "X")._core }

  let arrays = cores.map(Array.init)
  
  let contents = cores.map {
    String._Testable.UTF16View(legacy: $0)
  }

  var N = 20000
  _sanityCheck({ N = 1; return true }()) // Reset N for debug builds
  
  for (x, y) in zip(cores, contents) {
    if !x.elementsEqual(y) {
      debugPrint(String(x))
      dump(y)
      debugPrint(y)
      print(Array(x))
      print(Array(y))
      fatalError("unequal")
    }
    _sanityCheck(
      {
        debugPrint(String(x))
        dump(y)
        print()
        return true
      }())
  }

  var total = 0
  @inline(never)
  func lexicographicalComparison_new() {
    time {
      for _ in 0...N {
        for a in contents {
          for b in contents {
            if a.lexicographicallyPrecedes(b) { total = total &+ 1 }
          }
        }
      }
    }
  }

  @inline(never)
  func lexicographicalComparison_old() {
    time {
      for _ in 0...N {
        for a in cores {
          for b in cores {
            if a.lexicographicallyPrecedes(b) { total = total &+ 1 }
          }
        }
      }
    }
  }
  lexicographicalComparison_old()
  lexicographicalComparison_new()
  print()
  
  @inline(never)
  func initFromArray_new() {
    time {
      for _ in 0...10*N {
        for a in arrays {
          total = total &+ String._Testable.UTF16View(a).count
        }
      }
    }
  }
  
  @inline(never)
  func initFromArray_old() {
    time {
      for _ in 0...10*N {
        for a in arrays {
          total = total &+ _StringCore(a).count
        }
      }
    }
  }
  initFromArray_old()
  initFromArray_new()
  print()

  /*
  @inline(never)
  func concat3Iteration() {
    time {
      for _ in 0...100*N {
        for x in _Concat3(5..<90, 6...70, (4...30).dropFirst()) {
          total = total &+ x
        }
      }
    }
  }
  concat3Iteration()
  print()
  */
  
  let a_old = "a"._core
  let a_new = String._Testable.UTF16View(a_old)
  
  let short8_old = ["b","c","d","pizza"].map { $0._core }
  let short8_new = short8_old.map { String._Testable.UTF16View($0) }
  
  @inline(never)
  func  appendManyTinyASCIIFragments_ToASCII_old() {
    time {
      var sb = a_old
      for _ in 0...N*200 {
        for x in short8_old {
          sb.append(contentsOf: x)
        }
      }
      total = total &+ sb.count
    }
  }
  appendManyTinyASCIIFragments_ToASCII_old()
  
  @inline(never)
  func  appendManyTinyASCIIFragments_ToASCII_new() {
    time {
      var sb = a_new
      for _ in 0...N*200 {
        for x in short8_new {
          sb.append(contentsOf: x)
        }
      }
      total = total &+ sb.count
    }
  }
  appendManyTinyASCIIFragments_ToASCII_new()
  print()
  
  let short16_old = ["🎉","c","d","pizza"].map { $0._core }
  let short16_new = short16_old.map { String._Testable.UTF16View($0) }

  @inline(never)
  func  appendManyTinyFragmentsOfBothWidths_old() {
    time {
      var sb = a_old
      for _ in 0...N*300 {
        for x in short16_old {
          sb.append(contentsOf: x)
        }
      }
      total = total &+ sb.count
    }
  }
  appendManyTinyFragmentsOfBothWidths_old()
  
  @inline(never)
  func  appendManyTinyFragmentsOfBothWidths_new() {
    time {
      var sb = a_new
      for _ in 0...N*300 {
        for x in short16_new {
          sb.append(contentsOf: x)
        }
      }
      total = total &+ sb.count
    }
  }
  appendManyTinyFragmentsOfBothWidths_new()
  print()
  
  let ghost_old = "👻"._core
  let ghost_new = String._Testable.UTF16View(ghost_old)
  
  let long_old = "Swift is a multi-paradigm, compiled programming language created for iOS, OS X, watchOS, tvOS and Linux development by Apple Inc. Swift is designed to work with Apple's Cocoa and Cocoa Touch frameworks and the large body of existing Objective-C code written for Apple products. Swift is intended to be more resilient to erroneous code (\"safer\") than Objective-C and also more concise. It is built with the LLVM compiler framework included in Xcode 6 and later and uses the Objective-C runtime, which allows C, Objective-C, C++ and Swift code to run within a single program."._core
  let long_new = String._Testable.UTF16View(long_old)
  
  @inline(never)
  func appendManyLongASCII_ToUTF16_old() {
    time {
      var sb = ghost_old
      for _ in 0...N*20 {
        sb.append(contentsOf: long_old)
      }
      total = total &+ sb.count
    }
  }
  appendManyLongASCII_ToUTF16_old()
  
  @inline(never)
  func appendManyLongASCII_ToUTF16_new() {
    time {
      var sb = ghost_new
      for _ in 0...N*20 {
        sb.append(contentsOf: long_new)
      }
      total = total &+ sb.count
    }
  }
  appendManyLongASCII_ToUTF16_new()
  print()
  
  @inline(never)
  func  appendFewTinyASCIIFragments_ToASCII_old() {
    time {
      for _ in 0...N*200 {
        var sb = a_old
        for x in short8_old {
          sb.append(contentsOf: x)
        }
        total = total &+ sb.count
      }
    }
  }
  appendFewTinyASCIIFragments_ToASCII_old()
  
  @inline(never)
  func  appendFewTinyASCIIFragments_ToASCII_new() {
    time {
      for _ in 0...N*200 {
        var sb = a_new
        for x in short8_new {
          sb.append(contentsOf: x)
        }
        total = total &+ sb.count
      }
    }
  }
  appendFewTinyASCIIFragments_ToASCII_new()
  print()
  
  @inline(never)
  func  appendFewTinyFragmentsOfBothWidths_old() {
    time {
      for _ in 0...N*300 {
        var sb = a_old
        for x in short16_old {
          sb.append(contentsOf: x)
        }
        total = total &+ sb.count
      }
    }
  }
  appendFewTinyFragmentsOfBothWidths_old()
  
  @inline(never)
  func  appendFewTinyFragmentsOfBothWidths_new() {
    time {
      for _ in 0...N*300 {
        var sb = a_new
        for x in short16_new {
          sb.append(contentsOf: x)
        }
        total = total &+ sb.count
      }
    }
  }
  appendFewTinyFragmentsOfBothWidths_new()
  print()
  
  @inline(never)
  func  appendOneLongASCII_ToUTF16_old() {
    time {
      for _ in 0...N*20 {
        var sb = ghost_old
        sb.append(contentsOf: long_old)
        total = total &+ sb.count
      }
    }
  }
  appendOneLongASCII_ToUTF16_old()
  
  @inline(never)
  func  appendOneLongASCII_ToUTF16_new() {
    time {
      for _ in 0...N*20 {
        var sb = ghost_new
        sb.append(contentsOf: long_new)
      }
    }
  }
  appendOneLongASCII_ToUTF16_new()
  print()
  
  if total == 0 { print() }
}

test_newStringCore(instrumentedWith: timer)

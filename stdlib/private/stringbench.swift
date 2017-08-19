// Compile with old and new compiler. Run old one with 1 argument, pipe results into new one
// time xcrun swiftc -O -o stringbench-shipping stdlib/private/stringbench.swift
// time xcrun ../build/Ninja-Release/swift-macosx-x86_64/bin/swiftc -O -o stringbench-branch stdlib/private/stringbench.swift
// ./stringbench-shipping - | ./stringbench-branch

extension String {
  @available(swift, obsoleted: 4) 
  var count: Int { return characters.count }
  
  @available(swift, obsoleted: 4) 
  func map<T>(_ transform: (String)->T) -> [T] {
    return characters.map { transform(String($0)) }
  }

  @available(swift, obsoleted: 4) 
  func split(separator: Character) -> [String] {
    return characters.split(separator: separator).map { String($0) }
  }
}

var words = [
  "James", "John", "Robert", "Michael", "William", "David", "Richard", "Joseph",
  "Charles", "Thomas", "Christopher", "Daniel", "Matthew", "Donald", "Anthony",
  "Paul", "Mark", "George", "Steven", "Kenneth", "Andrew", "Edward", "Brian",
  "Joshua", "Kevin", "Ronald", "Timothy", "Jason", "Jeffrey", "Gary", "Ryan",
  "Nicholas", "Eric", "Stephen", "Jacob", "Larry", "Frank", "Jonathan", "Scott",
  "Justin", "Raymond", "Brandon", "Gregory", "Samuel", "Patrick", "Benjamin",
  "Jack", "Dennis", "Jerry", "Alexander", "Tyler", "Douglas", "Henry", "Peter",
  "Walter", "Aaron", "Jose", "Adam", "Harold", "Zachary", "Nathan", "Carl",
  "Kyle", "Arthur", "Gerald", "Lawrence", "Roger", "Albert", "Keith", "Jeremy",
  "Terry", "Joe", "Sean", "Willie", "Jesse", "Ralph", "Billy", "Austin", "Bruce",
  "Christian", "Roy", "Bryan", "Eugene", "Louis", "Harry", "Wayne", "Ethan",
  "Jordan", "Russell", "Alan", "Philip", "Randy", "Juan", "Howard", "Vincent",
  "Bobby", "Dylan", "Johnny", "Phillip", "Craig",// "マイケル"
]
var allWords : String = words.reduce("") { return $0 + $1 }
let otherWords = words.reduce("") { return $0 + $1 }

import Dispatch
import Darwin

var previousTimes: [String:Int] = [:]
var newTimes: [String:Int] = [:]

while CommandLine.arguments.count == 1, let line = readLine() {
  let funcTime = line.split(separator: ",")
  guard let f = funcTime.first, let t = funcTime.last.flatMap({ Int(String($0)) }) else {
    fatalError("Bad record: \(line)")
  }
  previousTimes[String(f)] = t
}

func time<T>(_ _caller : String = #function, _ block: () -> T) -> T {
  let start = DispatchTime.now()
  let res = block()
  let end = DispatchTime.now()
  let milliseconds = (end.uptimeNanoseconds - start.uptimeNanoseconds) / 1_000_000
  if let previous = previousTimes[_caller] {
    print("\(_caller),\(previous),\(milliseconds)")    
  }
  else {
    print("\(_caller),\(milliseconds)")        
  }
  return res
}

@inline(never)
public func hashLargeString(_ N: Int) -> Int {
  return time {
    var hashValue = 0
    for _ in 0..<N {
      hashValue = allWords.hashValue
    }
   return hashValue
  }
}


@inline(never)
public func equalLargeString(_ N: Int) -> Int {
  let otherWords = words.reduce("") { return $0 + $1 }
  return time {
    var retValue = 0
    for _ in 0..<N {
      retValue = otherWords == allWords ? retValue + 1 : retValue
    }
   return retValue
  }
}

@inline(never)
public func lessThanLargeString(_ N: Int) -> Int {
  return time {
    var retValue = 0
    for _ in 0..<N {
      retValue = otherWords < allWords ? retValue + 1 : retValue
    }
   return retValue
  }
}

var firstWord = words.first!

@inline(never)
public func equalSmallString(_ N: Int) -> Int {
  let otherWord = words.first!
  return time {
    var retValue = 0
    for _ in 0..<N {
      retValue = firstWord == otherWord ? retValue + 1 : retValue
    }
   return retValue
  }
}

@inline(never)
public func countCharactersShort(_ N: Int) -> Int {
  let word = words.first!
  return time {
    var retValue = 0
    for _ in 0..<N {
      retValue += word.count
    }
   return retValue
  }
}

@inline(never)
public func countCharactersUTF16(_ N: Int) -> Int {
  let word = words.reduce("☀️") { return $0 + $1 }
  return time {
    var retValue = 0
    for _ in 0..<N {
      retValue += word.count
    }
   return retValue
  }
}

@inline(never)
public func countCharactersLatin1(_ N: Int) -> Int {
  let word = words.reduce("é") { return $0 + $1 }
  return time {
    var retValue = 0
    for _ in 0..<N {
      retValue += word.count
    }
   return retValue
  }
}

@inline(never)
public func countCharactersLong(_ N: Int) -> Int {
  let word = words.reduce("") { return $0 + $1 }
  return time {
    var retValue = 0
    for _ in 0..<N {
      retValue += word.count
    }
   return retValue
  }
}

@inline(never)
public func iterateCharactersLong(_ N: Int) -> Int {
  let word = words.reduce("") { return $0 + $1 }
  let c: Character = "e"
  return time {
    var count = 0
    for _ in 0..<N {
      var i = word.startIndex
      while i != word.endIndex {
        if word[i] == c {
          count += 1
          break
        }
        i = word.index(after: i)
      }
    }
   return count
  }
}

@inline(never)
public func iterateCharactersUTF16(_ N: Int) -> Int {
  let word = words.reduce("☀️") { return $0 + $1 }
  let c: Character = "e"
  return time {
    var count = 0
    for _ in 0..<N {
      var i = word.startIndex
      while i != word.endIndex {
        if word[i] == c {
          count += 1
          break
        }
        i = word.index(after: i)
      }
    }
   return count
  }
}


@inline(never)
public func iterateCharactersShort(_ N: Int) -> Int {
  let word = words.first!
  let c: Character = "e"
  return time {
    var count = 0
    for _ in 0..<N {
      var i = word.startIndex
      while i != word.endIndex {
        if word[i] == c {
          count += 1
          break
        }
        i = word.index(after: i)
      }
    }
   return count
  }
}

@inline(never)
public func appendLong(_ N: Int) -> String {
  let word = words.reduce("") { return $0 + $1 }
  return time {
    var result = ""
    for _ in 0..<N {
      result += word
    }
    return result
  }
}

@inline(never)
public func appendShort(_ N: Int) -> String {
  let word = words.first!
  return time {
    var result = ""
    for _ in 0..<N {
      result += word
    }
    return result
  }
}

@inline(never)
public func isEmptyShort(_ N: Int) -> Int {
  let word = words.first!
  return time {
    var count = 0
    for _ in 0..<N {
      if !word.isEmpty { count += 1 }
    }
    return count
  }
}

@inline(never)
public func isEmptyLong(_ N: Int) -> Int {
  let word = words.reduce("") { return $0 + $1 }
  return time {
    var count = 0
    for _ in 0..<N {
      if !word.isEmpty { count += 1 }
    }
    return count
  }
}


@inline(never)
public func withCString(_ N: Int) -> UInt {
  let word = words.reduce("") { return $0 + $1 }
  return time {
    var count: UInt = 0
    for _ in 0..<N {
      count += word.withCString(strlen) % 100
    }
    return count
  }
}

@inline(never)
public func withCStringUTF16(_ N: Int) -> UInt {
  let word = words.reduce("☀️") { return $0 + $1 }
  return time {
    var count: UInt = 0
    for _ in 0..<N {
      count += word.withCString(strlen) % 100
    }
    return count
  }
}


@inline(never)
public func fromCString(_ N: Int) -> Int {
  let word = words.reduce("") { return $0 + $1 }
  let cstr = word.withCString(strdup)
  defer { free(cstr) }
  return time {
    var count = 0
    for _ in 0..<N {
      _ = String(cString: cstr!)
      count += 1
    }
    return count
  }
}


_ = hashLargeString(100_000)
_ = equalLargeString(500_000)
_ = equalSmallString(1_000_000)
_ = lessThanLargeString(100_000)
_ = countCharactersShort(100_000)
_ = countCharactersLong(100_000)
_ = countCharactersUTF16(1_000)
_ = countCharactersLatin1(10_000)
_ = iterateCharactersLong(100_000)
_ = iterateCharactersUTF16(1_000)
_ = iterateCharactersShort(100_000)
_ = appendLong(10_000)
_ = appendShort(100_000)
_ = isEmptyLong(1_000_000)
_ = isEmptyShort(1_000_000)
_ = withCString(10_000)
_ = withCStringUTF16(10_000)
_ = fromCString(10_000)



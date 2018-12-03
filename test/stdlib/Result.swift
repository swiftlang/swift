// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import Swift

let ResultTests = TestSuite("Result")

fileprivate enum Err: Error, Equatable {
  case err
  case derr
}

fileprivate let string = "string"

fileprivate extension Result {
  var value: Value? {
    switch self {
    case let .value(value):
      return value
    case .error:
      return nil
    }
    
    var error: Error? {
      switch self {
      case .value:
        return nil
      case let .error(error):
        return error
      }
    }
  }
}

ResultTests.test("Construction") {
  let result1: Result<String, Err> = .value(string)
  let result2: Result<String, Err> = .error(.err)
  let string1: String? = {
    switch result1 {
      case let .value(string): 
        return string
      case .error: 
        expectUnreachable()
        return nil
    }
  }()
  let error: Err? = {
    switch result2 {
      case let .error(error): 
        return error
      case .value: 
        expectUnreachable()
        return nil
    }
  }()

  expectEqual(string1, string)
  expectEqual(error, .err)
}

ResultTests.test("Throwing Initialization and Unwrapping") {
  func notThrowing() throws -> String {
    return string
  }

  func throwing() throws -> String {
    throw Err.err
  }
    
  let result1 = Result { try throwing() }
  let result2 = Result { try notThrowing() }
  
  // Kept getting enum case 'error' cannot be used as an instance member, so get value manually.
  switch result1 {
  case let .error(error):
    expectEqual(error as? Err, Err.err)
  case .value:
    expectUnreachable() 
  }
  
  expectEqual(result2.value, string)
    
  do {
    _ = try result1.unwrapped()
  } catch let error as Err {
    expectEqual(error, Err.err)
  } catch {
    expectUnreachable()
  }
    
  do {
    let unwrapped = try result2.unwrapped()
    expectEqual(unwrapped, string)
  } catch {
    expectUnreachable()
  }
    
  // Test unwrapping strongly typed error.
  let result3 = Result<String, Err>.error(Err.err)
  do {
    _ = try result3.unwrapped()
  } catch let error as Err {
    expectEqual(error, Err.err)
  } catch {
    expectUnreachable()
  }
}

ResultTests.test("Functional Transforms") {
  func transformDouble(_ int: Int) -> Int {
    return 2 * int
  }
  
  func transformTriple(_ int: Int) -> Int {
    return 3 * int
  }
  
  func transformError(_ err: Err) -> Err {
    if err == .err {
      return .derr
    } else {
      return .err
    }
  }

  func resultValueTransform(_ int: Int) -> Result<Int, Err> {
    return .value(transformDouble(int))
  }
  
  func resultErrorTransform(_ err: Err) -> Result<Int, Err> {
    return .error(transformError(err))
  }
    
  let result1: Result<Int, Err> = .value(1)
  let newResult1 = result1.map(transformDouble)
    
  expectEqual(newResult1, .value(2))
    
  let result2: Result<Int, Err> = .error(.err)
  let newResult2 = result2.mapError(transformError)
    
  expectEqual(newResult2, .error(.derr))
    
  let result3: Result<Int, Err> = .value(1)
  let newResult3 = result3.flatMap(resultValueTransform)
    
  expectEqual(newResult3, .value(2))
    
  let result4: Result<Int, Err> = .error(.derr)
  let newResult4 = result4.flatMapError(resultErrorTransform)
    
  expectEqual(newResult4, .error(.err))
}

ResultTests.test("Equatable") {
  let result1: Result<Int, Err> = .value(1)
  let result2: Result<Int, Err> = .error(.err)

  expectEqual(result1, .value(1))
  expectNotEqual(result1, .value(2))
  expectNotEqual(result1, .error(.err))
  expectNotEqual(result1, .error(.derr))

  expectNotEqual(result2, .value(1))
  expectNotEqual(result2, .value(2))
  expectEqual(result2, .error(.err))
  expectNotEqual(result2, .error(.derr))
}

ResultTests.test("Hashable") {
  let result1: Result<Int, Err> = .value(1)
  let result2: Result<Int, Err> = .value(2)
  let result3: Result<Int, Err> = .error(.err)
  checkHashable([result1, result2, result3], equalityOracle: { $0 == $1 })
}

runAllTests()

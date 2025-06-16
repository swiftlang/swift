// REQUIRES: swift_swift_parser, executable_test

// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking -parse-as-library -enable-experimental-feature Macros -Xfrontend -plugin-path -Xfrontend %swift-plugin-dir)

// Run this test via the swift-plugin-server
// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking -parse-as-library -enable-experimental-feature Macros -Xfrontend -external-plugin-path -Xfrontend %swift-plugin-dir#%swift-plugin-server)

// REQUIRES: observation
// REQUIRES: concurrency
// REQUIRES: objc_interop
// REQUIRES: swift_feature_Macros
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import StdlibUnittest
import Observation

@Observable
final class TestModel {
  var value: Int = 0
  var name: String = "test"
}

@Observable
final class AsyncTestModel {
  var count: Int = 0
  
  func increment() async {
    await Task.yield() // Simulate async work
    count += 1
  }
}

enum TestError: Error, Equatable {
  case simulatedError
  case anotherError
}

@main
struct Validator {
  @MainActor
  static func main() {

    
    let suite = TestSuite("Observations")

    suite.test("Basic element emission") {
    	let model = TestModel()
	    var emissionCount = 0
	    
	    let observations = Observations<Int, Never> {
	      model.value
	    }
	    
	    var iterator = observations.makeAsyncIterator()
	    
	    // Get first value (initial)
	    let first = await iterator.next()
	    #expect(first == 0)
	    
	    // Modify the model in a separate task
	    Task {
	      await Task.yield()
	      model.value = 42
	    }
	    
	    // Get second value (after change)
	    let second = await iterator.next()
	    #expect(second == 42)
    }

    suite.test("Multiple property tracking") {
    	let model = TestModel()
    
	    let observations = Observations<String, Never> {
	      "\(model.name):\(model.value)"
	    }
	    
	    var iterator = observations.makeAsyncIterator()
	    
	    // Get initial value
	    let first = await iterator.next()
	    #expect(first == "test:0")
	    
	    // Modify both properties
	    Task {
	      await Task.yield()
	      model.name = "updated"
	      model.value = 100
	    }
	    
	    let second = await iterator.next()
	    #expect(second == "updated:100")
    }

    suite.test("Error handling in emit closure") {
    	let model = TestModel()
	    var shouldThrow = false
	    
	    let observations = Observations<Int, TestError> { () throws(TestError) -> Int in
	      if shouldThrow {
	        throw TestError.simulatedError
	      }
	      return model.value
	    }
	    
	    var iterator = observations.makeAsyncIterator()
	    
	    // First iteration should succeed
	    let first = try await iterator.next()
	    #expect(first == 0)
	    
	    // Enable error throwing and trigger change
	    Task {
	      await Task.yield()
	      shouldThrow = true
	      model.value = 42
	    }
	    
	    // Second iteration should throw
	    do {
	      _ = try await iterator.next()
	      Issue.record("Expected error to be thrown")
	    } catch TestError.simulatedError {
	      // Expected error
	    } catch {
	      Issue.record("Unexpected error type: \(error)")
	    }
	    
	    // Sequence should be terminated after error
	    let afterError = try await iterator.next()
	    #expect(afterError == nil)
    }

    suite.test("Concurrent observers") {
    	let model = TestModel()
    
	    let observations = Observations<Int, Never> {
	      model.value
	    }
	    
	    // Create multiple concurrent iterators
	    let task1 = Task {
	      var iterator = observations.makeAsyncIterator()
	      var values: [Int] = []
	      for _ in 0..<3 {
	        if let value = await iterator.next() {
	          values.append(value)
	        }
	      }
	      return values
	    }
	    
	    let task2 = Task {
	      var iterator = observations.makeAsyncIterator()
	      var values: [Int] = []
	      for _ in 0..<3 {
	        if let value = await iterator.next() {
	          values.append(value)
	        }
	      }
	      return values
	    }
	    
	    // Trigger changes
	    let changeTask = Task {
	      await Task.yield()
	      model.value = 100
	      await Task.yield()
	      model.value = 200
	    }
	    
	    let (result1, result2) = await (task1.value, task2.value)
	    await changeTask.value
	    
	    // Both iterators should see the same progression of values
	    #expect(result1.count == 3)
	    #expect(result2.count == 3)
	    #expect(result1[0] == 0) // Initial value
	    #expect(result2[0] == 0) // Initial value
    }

    suite.test("Task cancellation behavior") {
    	let model = TestModel()
	    
	    let observations = Observations<Int, Never> {
	      model.value
	    }
	    
	    let task = Task {
	      var iterator = observations.makeAsyncIterator()
	      var values: [Int] = []
	      
	      // Get initial value
	      if let value = await iterator.next() {
	        values.append(value)
	      }
	      
	      // This should be cancelled before getting next value
	      if let value = await iterator.next() {
	        values.append(value)
	      }
	      
	      return values
	    }
	    
	    // Let the task start and get the first value
	    await Task.yield()
	    
	    // Cancel the task
	    task.cancel()
	    
	    let result = await task.value
	    
	    // Should only have gotten the initial value before cancellation
	    #expect(result.count == 1)
	    #expect(result[0] == 0)
    }

    suite.test("Rapid successive changes") {
    	let model = TestModel()
    
	    let observations = Observations<Int, Never> {
	      model.value
	    }
	    
	    var iterator = observations.makeAsyncIterator()
	    
	    // Get initial value
	    let first = await iterator.next()
	    #expect(first == 0)
	    
	    // Make rapid changes
	    Task {
	      await Task.yield()
	      for i in 1...5 {
	        model.value = i
	      }
	    }
	    
	    // Should get the final state after all rapid changes
	    let second = await iterator.next()
	    #expect(second == 5)
    }

    suite.test("Empty sequence behavior") {
    	// Test sequence that immediately finishes
	    let observations = Observations<Int, Never>.untilFinished {
	      return .finish
	    }
	    
	    var iterator = observations.makeAsyncIterator()
	    let result = await iterator.next()
	    #expect(result == nil)
    }

    runAllTests()
  }
}
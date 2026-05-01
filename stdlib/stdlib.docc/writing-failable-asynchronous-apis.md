# Writing Failable Asynchronous APIs

Vend results as part of an API when you can’t return errors synchronously.

## Overview

When writing a function, method, or other API that might fail, you use the `throws`
keyword on the declaration to indicate that the API call can throw an error. However,
you can’t use the `throws` keyword to model APIs that return asynchronously. Instead,
use the ``Swift/Result`` enumeration to capture information about whether an asychronous
call succeeds or fails, and use the associated values for the ``Swift/Result/success(_:)``
and ``Swift/Result/failure(_:)`` cases to carry information about the result of the
call.

### Return Result Instances Asynchronously

The following example models an asynchronous source of random numbers. The `fetchRemoteRandomNumber(completion:)`
method returns `Void` synchronously, and asynchronously calls a completion handler
with a `Result<Int, EntropyError>` instance that contains either a random result
or information about the failure.

```swift
let queue = DispatchQueue(label: "com.example.queue")

enum EntropyError: Error {
    case entropyDepleted
}

struct AsyncRandomGenerator {
    static let entropyLimit = 5
    var count = 0

    mutating func fetchRemoteRandomNumber(
        completion: @escaping (Result<Int, EntropyError>) -> Void
    ) {
        let result: Result<Int, EntropyError>
        if count < AsyncRandomGenerator.entropyLimit {
            // Produce numbers until reaching the entropy limit.
            result = .success(Int.random(in: 1...100))
        } else {
            // Supply a failure reason when the caller hits the limit.
            result = .failure(.entropyDepleted)
        }

        count += 1

        // Delay to simulate an asynchronous source of entropy.
        queue.asyncAfter(deadline: .now() + 2) {
            completion(result)
        }
    }
}
```

Users of your remote random number generator can decide how to handle both the success
and failure cases:

```swift
var generator = AsyncRandomGenerator()

// Request one more number than the limit to trigger a failure.
(0..<AsyncRandomGenerator.entropyLimit + 1).forEach { _ in
    generator.fetchRemoteRandomNumber { result in
        switch result {
        case .success(let number):
            print(number)
        case .failure(let error):
            print("Source of randomness failed: \(error)")
        }
    }
}

print("Waiting on some numbers.")

dispatchMain()
/* Prints:
success(29)
success(46)
success(85)
success(39)
success(84)
Source of randomness failed: entropyDepleted
*/
```

# Unused throwing unstructured task creation (NoUseUnstructuredThrowingTask)

## Overview

Creating a throwing unstructured `Task` without storing or discarding its result means any error thrown 
by the task body will be silently ignored. Resolve this warning by explicitly storing or discarding the 
task reference.

For example:

```swift
func example() {
  Task { throw MyError() }
}
```

Building this code produces a warning about the unused unstructured task:

```
| func example() {
|   Task { throw MyError() }
|   `- warning: unstructured throwing task created by 'init(name:priority:operation:)' is not used, which may accidentally ignore errors thrown inside the task
|      note: to silence this warning, handle the error inside the task, or store/discard the task value explicitly
| }
```

By storing the task reference and awaiting it, you won't miss the error thrown by the task operation because awaiting `task.value` will be forced to acknowledge the thrown error with `try`:

```swift
let task = Task { throw MyError() }
try await task.value
```

Alternatively, ignore the thrown error explicitly:

```swift
_ = Task { throw MyError() }
```

### Cancelling unstructured tasks

Another reason to keep references to unstructured tasks is to be able to cancel them explicitly:

```swift
let task = Task { try Task.sleep(for: .seconds(10)) }
task.cancel() // cancel and wake-up the sleeping task
try await task.value
```

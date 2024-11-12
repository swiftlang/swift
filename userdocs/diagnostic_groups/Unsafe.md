# Unsafe code wanings (`Unsafe`)


This diagnostic group includes warnings that identify the use of unsafe language constructs and library APIs. Example warnings include:

- Use of an unsafe language feature:
  ```swift
  // warning: unowned(unsafe) involves unsafe code
  unowned(unsafe) var parentNode: TreeNode<T>
  ```
- Use of a function or type annotated with `@unsafe`:
  ```swift
  // warning: reference to unsafe generic struct 'UnsafeMutablePointer'
  func getPointee<T>(_ pointer: UnsafeMutablePointer<Int>, as type: T.Type) -> T {
    // warning: call to unsafe global function 'unsafeBitCast'
    return unsafeBitCast(pointer.pointee, to: type)
  }
  ```
- Use of a function involving an `@unsafe` type:
  ```swift
  func evilMalloc(size: Int) -> Int {
    // warning: call to global function 'malloc' involves unsafe type 'UnsafeMutableRawPointer'
    return Int(bitPattern: malloc(size))
  }
  ```

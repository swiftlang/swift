import CTensorFlow
import TensorFlow

/// The `TF_Status *` type.
@usableFromInline typealias CTFStatus = OpaquePointer

/// The `TF_Function*` type.
typealias CTFFunction = OpaquePointer

@usableFromInline
func internalConsistencyCheck(
  _ predicate: Bool,
  _ errMessage: String = "TF runtime assertion failure",
  file: StaticString = #file,
  line: UInt = #line
) {
  guard predicate else {
    fatalError(errMessage, file: file, line: line)
  }
}

@usableFromInline
func checkOk(_ s: CTFStatus?, file: StaticString = #file, line: UInt = #line) {
  internalConsistencyCheck(TF_GetCode(s) == TF_OK,
                           String(cString: TF_Message(s)),
                           file: file, line: line)
}

@usableFromInline
func debugLog(_ message: @autoclosure () -> String,
              file: StaticString = #file,
              line: UInt = #line) {
  if _RuntimeConfig.printsDebugLog {
    print("[\(file):\(line)] \(message())")
    // This helps dump more log before a crash.
    fflush(stdout)
  }
}

//////////////////////////////////
// TO move to TensorFlow stdlib
//////////////////////////////////

// public enum _RuntimeConfigTmp {
//   fileprivate static var traceState: TracingState = .notTracing
// }

public func foo() {
  let x = Tensor<Float>(1.0)
  _ = x + x
}

_RuntimeConfig.printsDebugLog = true
let tracedFn = trace(foo)
tracedFn()


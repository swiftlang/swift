import Foundation

private let symbolicationPath =
  "/System/Library/PrivateFrameworks/Symbolication.framework/Symbolication"
private let symbolicationHandle = dlopen(symbolicationPath, RTLD_LAZY)!

private let coreSymbolicationPath =
  "/System/Library/PrivateFrameworks/Symbolication.framework/Symbolication"
private let coreSymbolicationHandle = dlopen(coreSymbolicationPath, RTLD_LAZY)!

private func symbol<T>(_ handle: UnsafeMutableRawPointer, _ name: String) -> T {
  guard let result = dlsym(handle, name) else {
    fatalError("Unable to look up \(name) in Symbolication")
  }
  return unsafeBitCast(result, to: T.self)
}

typealias CSTypeRef = NSRange
typealias CSMachineTime = UInt64
let kCSNow = CSMachineTime(Int64.max) + 1

func pidFromHint(_ hint: String) -> pid_t? {
  let pidFromHint: @convention(c) (NSString) -> pid_t =
    symbol(symbolicationHandle, "pidFromHint")
  let result = pidFromHint(hint as NSString)
  return result == 0 ? nil : result
}

func CSSymbolicatorCreateWithTask(_ task: task_t) -> CSTypeRef {
  let CSSymbolicatorCreateWithTask: @convention(c) (task_t) -> CSTypeRef =
    symbol(coreSymbolicationHandle, "CSSymbolicatorCreateWithTask")
  return CSSymbolicatorCreateWithTask(task)
}

func CSSymbolicatorGetSymbolOwnerWithNameAtTime(_ symbolicator: CSTypeRef,
                                                _ name: String,
                                                _ time: CSMachineTime)
                                                -> CSTypeRef {
  let CSSymbolicatorGetSymbolOwnerWithNameAtTime:
    @convention(c) (CSTypeRef, UnsafePointer<CChar>, CSMachineTime) -> CSTypeRef =
      symbol(coreSymbolicationHandle, "CSSymbolicatorGetSymbolOwnerWithNameAtTime")
  return CSSymbolicatorGetSymbolOwnerWithNameAtTime(symbolicator, name, time)
}

@discardableResult
func CSSymbolOwnerForeachSymbol(_ symbolOwner: CSTypeRef,
                                _ iterator: (CSTypeRef) -> Void) -> UInt {
  let CSSymbolOwnerForeachSymbol:
    @convention(c) (CSTypeRef, @convention(block) (CSTypeRef) -> Void) -> UInt =
      symbol(coreSymbolicationHandle, "CSSymbolOwnerForeachSymbol")
  return CSSymbolOwnerForeachSymbol(symbolOwner, iterator)
}

func CSSymbolGetName(_ sym: CSTypeRef) -> String? {
  let CSSymbolGetName: @convention(c) (CSTypeRef) -> UnsafePointer<CChar>? =
    symbol(coreSymbolicationHandle, "CSSymbolGetName")
  let name = CSSymbolGetName(sym)
  return name.map({ String(cString: $0) })
}

func CSSymbolGetMangledName(_ sym: CSTypeRef) -> String? {
  let CSSymbolGetMangledName: @convention(c) (CSTypeRef) -> UnsafePointer<CChar>? =
    symbol(coreSymbolicationHandle, "CSSymbolGetMangledName")
  let name = CSSymbolGetMangledName(sym)
  return name.map({ String(cString: $0) })
}

func CSSymbolIsFunction(_ sym: CSTypeRef) -> Bool {
  let CSSymbolIsFunction: @convention(c) (CSTypeRef) -> CBool =
    symbol(coreSymbolicationHandle, "CSSymbolIsFunction")
  return CSSymbolIsFunction(sym)
}

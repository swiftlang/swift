//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#if SWIFT_WASM_USE_JSC

import JavaScriptCore
import WASI
import WasmTypes
import SystemPackage

typealias DefaultWasmEngine = JSCWasmEngine

// (wasm: ArrayBuffer, imports: Object) => Promise<{ ... }>
private let js = """
async (wasmData, imports) => {
  const mod = await WebAssembly.compile(wasmData);
  const instance = await WebAssembly.instantiate(mod, imports);
  const api = instance.exports;
  return {
    api,
    read: (off, size) => new Uint8Array(new Uint8Array(api.memory.buffer, off, size)).buffer,
    write: (buf, off, size) => new Uint8Array(api.memory.buffer, off, size).set(new Uint8Array(buf)),
  };
}
"""

final class JSCWasmEngine: WasmEngine {
  private let runner: JSValue
  private let api: JSValue

  init(path: FilePath, imports: WASIBridgeToHost) throws {
    let factory = try JSCWasmFactory.shared
    let context = factory.context

    var memory: JSCGuestMemory?
    let imports: [String: [String: JSValue]] = imports.wasiHostModules.mapValues { module in
      module.functions.mapValues { function in
        function.asJSFunction(in: context) {
          guard let memory else { throw JSCWasmError(message: "Missing GuestMemory") }
          return memory
        }
      }
    }

    let buffer = try NSData(contentsOfFile: path.string, options: .mappedIfSafe)
    let jsBuf = try JSValue(
      // JavaScript doesn't have readonly ArrayBuffers but all the JS we're running
      // is contained within this file and we know that we aren't mutating this buffer.
      // If we did attempt to mutate it, it would be UB.
      newArrayBufferWithBytesNoCopy: UnsafeMutableRawBufferPointer(
        start: UnsafeMutableRawPointer(mutating: buffer.bytes),
        count: buffer.count
      ),
      deallocator: { [box = buffer as AnyObject] in _ = box },
      in: context
    )
    let promise = factory.value.call(withArguments: [jsBuf, imports])
    guard let promise, context.exception == nil else {
      throw JSCWasmError(message: "Failed to load plugin", value: context.exception)
    }

    runner = try promise.synchronousPromiseValue()
    api = runner.objectForKeyedSubscript("api")!

    let getMemory = runner.objectForKeyedSubscript("read")!
    let setMemory = runner.objectForKeyedSubscript("write")!
    memory = JSCGuestMemory(getMemory: getMemory, setMemory: setMemory)
  }

  func function(named name: String) throws -> WasmFunction? {
    guard let export = api.objectForKeyedSubscript(name), export.isObject else {
      return nil
    }
    return { [api] args in
      let result = export.call(withArguments: args)
      if let exception = api.context.exception {
        throw JSCWasmError(message: "Call to '\(name)' failed", value: exception)
      }
      guard let result else { return [] }
      return result.isUndefined ? [] : [result.toUInt32()]
    }
  }
}

private struct JSCGuestMemory: GuestMemory {
  let getMemory: JSValue
  let setMemory: JSValue

  func withUnsafeMutableBufferPointer<T>(
    offset: UInt,
    count: Int,
    _ body: (UnsafeMutableRawBufferPointer) throws -> T
  ) rethrows -> T {
    // note that we can't get a shared pointer to the original wasm memory
    // because JSC doesn't allow JSObjectGetArrayBufferBytesPtr on Wasm memory.
    // we perform targeted copies instead.
    let memory = getMemory.call(withArguments: [offset, count])!
    defer { setMemory.call(withArguments: [memory, offset, count]) }
    return try memory.withUnsafeArrayBuffer(body)
  }
}

private struct JSCWasmFactory {
  let context: JSContext
  let value: JSValue

  // NB: the VM must be created on the MainActor for promises to work.
  private init() throws {
    let vm = JSVirtualMachine()
    guard let context = JSContext(virtualMachine: vm) else {
      throw JSCWasmError(message: "Failed to load plugin")
    }
    self.context = context
    self.value = context.evaluateScript(js)
  }

  private static let _shared = Result {
    try JSCWasmFactory()
  }

  static var shared: JSCWasmFactory {
    get throws {
      try _shared.get()
    }
  }
}

public struct JSCWasmError: Error, CustomStringConvertible {
  public let value: JSValue?
  public let description: String
  public init(message: String, value: JSValue? = nil) {
    self.value = value
    self.description = "\(message)\(value.map { ": \($0)" } ?? "")"
  }
}

extension WASIHostFunction {
  fileprivate func asJSFunction(in context: JSContext, memory: @escaping () throws -> GuestMemory) -> JSValue {
    JSValue(object: {
      let arguments = JSContext.currentArguments() as? [JSValue] ?? []
      let types: [Value] = zip(arguments, type.parameters).map { argument, type in
        switch type {
        case .i32: .i32(argument.toUInt32())
        case .i64: .i64(UInt64(argument.toString()) ?? 0)
        case .f32, .f64, .ref: .i32(0) // WASI doesn't support these
        }
      }
      do {
        let memory = try memory()
        if let result = try self.implementation(memory, types).first {
          return JSValue(uInt32: result.i32, in: context)
        }
      } catch {
        context.exception = JSValue(object: "\(error)", in: context)
      }
      return JSValue(undefinedIn: context)
    } as @convention(block) () -> JSValue, in: context)!
  }
}

extension JSValue {
  fileprivate convenience init(
    newArrayBufferWithData data: Data,
    in context: JSContext
  ) throws {
    let copy = UnsafeMutableBufferPointer<UInt8>.allocate(capacity: data.count)
    _ = copy.initialize(from: data)
    let address = UInt(bitPattern: copy.baseAddress)
    try self.init(
      newArrayBufferWithBytesNoCopy: UnsafeMutableRawBufferPointer(copy),
      deallocator: { UnsafeMutableRawPointer(bitPattern: address)?.deallocate() },
      in: context
    )
  }

  fileprivate convenience init(
    newArrayBufferWithBytesNoCopy bytes: UnsafeMutableRawBufferPointer,
    deallocator: @escaping @Sendable () -> Void,
    in context: JSContext
  ) throws {
    let box = Unmanaged.passRetained(deallocator as AnyObject).toOpaque()
    let rawBuf = JSObjectMakeArrayBufferWithBytesNoCopy(
      context.jsGlobalContextRef,
      bytes.baseAddress, bytes.count,
      { _, box in (Unmanaged<AnyObject>.fromOpaque(box!).takeRetainedValue() as! () -> Void)() },
      box,
      nil
    )
    self.init(jsValueRef: rawBuf, in: context)
  }

  // it's unsafe to call JavaScriptCore APIs inside the `perform` block as this
  // might invalidate the buffer.
  fileprivate func withUnsafeArrayBuffer<Result>(
    _ perform: (UnsafeMutableRawBufferPointer) throws -> Result
  ) rethrows -> Result {
    let rawContext = context.jsGlobalContextRef
    guard let base = JSObjectGetArrayBufferBytesPtr(rawContext, jsValueRef, nil) else {
      return try perform(UnsafeMutableRawBufferPointer(start: nil, count: 0))
    }
    let count = JSObjectGetArrayBufferByteLength(rawContext, jsValueRef, nil)
    let buffer = UnsafeMutableRawBufferPointer(start: base, count: count)
    return try perform(buffer)
  }

  fileprivate func arrayBufferData() -> Data {
    withUnsafeArrayBuffer { Data($0) }
  }

  private func getPromise(completion: @escaping (Result<JSValue, Error>) -> Void) {
    typealias Handler = @convention(block) (JSValue) -> Void
    let successFunc = JSValue(
      object: {
        completion(.success($0))
      } as Handler,
      in: context
    )
    let rejectFunc = JSValue(
      object: { error in
        completion(.failure(
          JSCWasmError(message: "Promise rejected", value: error)
        ))
      } as @convention(block) (JSValue) -> Void,
      in: context
    )
    guard let successFunc, let rejectFunc else {
      completion(.failure(JSCWasmError(message: "Could not await promise")))
      return
    }
    invokeMethod("then", withArguments: [successFunc, rejectFunc])
    if let exception = context.exception {
      completion(.failure(JSCWasmError(message: "Promise.then threw", value: exception)))
    }
  }

  func synchronousPromiseValue() throws -> JSValue {
    var result: Result<JSValue, Error>?
    getPromise {
      result = $0
    }
    while true {
      if let result { return try result.get() }
      RunLoop.main.run(until: Date(timeIntervalSinceNow: 0.01))
    }
  }
}

#endif

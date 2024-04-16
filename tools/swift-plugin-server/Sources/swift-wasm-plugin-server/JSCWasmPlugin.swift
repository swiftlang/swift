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

#if os(macOS)

import JavaScriptCore

typealias DefaultWasmPlugin = JSCWasmPlugin

// returns: (wasm: ArrayBuffer) => Promise<(input: ArrayBuffer) => ArrayBuffer>
// FIXME: stubbing WASI by always returning 0 triggers UB in the guest.
// (e.g. if we return 0 from args_sizes_get we must populate the out pointers.)
// we could adapt WasmKit.WASI to work with the JSC engine, since the host
// function code should be runtime-agnostic.
private let js = """
async (wasmData) => {
  const mod = await WebAssembly.compile(wasmData);
  // stub WASI imports
  const imports = WebAssembly.Module.imports(mod)
    .filter(x => x.module === "wasi_snapshot_preview1")
    .map(x => [x.name, () => {}]);
  const instance = await WebAssembly.instantiate(mod, {
    wasi_snapshot_preview1: Object.fromEntries(imports)
  });
  const api = instance.exports;
  api._start();
  return (json) => {
    const inAddr = api.wacro_malloc(json.byteLength);
    const mem = api.memory;
    const arr = new Uint8Array(mem.buffer, inAddr, json.byteLength);
    arr.set(new Uint8Array(json));
    const outAddr = api.wacro_parse(inAddr, json.byteLength);
    const len = new Uint32Array(mem.buffer, outAddr)[0];
    const outArr = new Uint8Array(mem.buffer, outAddr + 4, len);
    const copy = new Uint8Array(outArr);
    api.wacro_free(outAddr);
    return copy.buffer;
  }
}
"""

struct JSCWasmPlugin: WasmPlugin {
  private let handler: JSValue

  init(wasm data: Data) async throws {
    guard #available(macOS 10.15, *) else {
      throw JSCWasmError(message: "JSC Wasm plugins currently require macOS 10.15+")
    }

    let factory = try await JSCWasmFactory.shared
    let context = factory.context

    let jsBuf = try JSValue(newBufferWithData: data, in: context)
    guard let promise = factory.value.call(withArguments: [jsBuf]), context.exception == nil else {
      throw JSCWasmError(message: "Failed to load plugin", value: context.exception)
    }

    handler = try await promise.promiseValue
  }

  func handleMessage(_ json: Data) throws -> Data {
    guard let context = handler.context else {
      throw JSCWasmError(message: "Failed to invoke plugin")
    }
    let jsonJS = try JSValue(newBufferWithData: json, in: context)
    guard let result = handler.call(withArguments: [jsonJS]) else {
      throw JSCWasmError(message: "Wasm plugin did not provide a valid response", value: context.exception)
    }
    return result.arrayBufferData()
  }
}

@available(macOS 10.15, *)
private struct JSCWasmFactory {
  let context: JSContext
  let value: JSValue

  private init() async throws {
    // the VM must be created on the MainActor for async methods to work.
    let vm = await MainActor.run { JSVirtualMachine() }
    guard let context = JSContext(virtualMachine: vm) else {
      throw JSCWasmError(message: "Failed to load plugin")
    }
    self.context = context
    self.value = context.evaluateScript(js)
  }

  private static let _shared = Task {
    try await JSCWasmFactory()
  }

  static var shared: JSCWasmFactory {
    get async throws {
      try await _shared.value
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

extension JSValue {
  fileprivate convenience init(newBufferWithData data: Data, in context: JSContext) throws {
    let copy = UnsafeMutableBufferPointer<UInt8>.allocate(capacity: data.count)
    _ = copy.initialize(from: data)
    let rawBuf = JSObjectMakeArrayBufferWithBytesNoCopy(
      context.jsGlobalContextRef,
      copy.baseAddress, copy.count,
      { buf, _ in buf?.deallocate() }, nil,
      nil
    )
    self.init(jsValueRef: rawBuf, in: context)
  }

  // it's unsafe to call JavaScriptCore APIs inside the `perform` block as this
  // might invalidate the buffer.
  fileprivate func withUnsafeArrayBuffer<Result>(
    _ perform: (UnsafeRawBufferPointer) throws -> Result
  ) rethrows -> Result {
    let rawContext = context.jsGlobalContextRef
    guard let base = JSObjectGetArrayBufferBytesPtr(rawContext, jsValueRef, nil) else {
      return try perform(UnsafeRawBufferPointer(start: nil, count: 0))
    }
    let count = JSObjectGetArrayBufferByteLength(rawContext, jsValueRef, nil)
    let buffer = UnsafeRawBufferPointer(start: base, count: count)
    return try perform(buffer)
  }

  fileprivate func arrayBufferData() -> Data {
    withUnsafeArrayBuffer { Data($0) }
  }

  @available(macOS 10.15, *)
  fileprivate var promiseValue: JSValue {
    get async throws {
      try await withCheckedThrowingContinuation { continuation in
        typealias Handler = @convention(block) (JSValue) -> Void
        let successFunc = JSValue(
          object: {
            continuation.resume(returning: $0)
          } as Handler,
          in: context
        )
        let rejectFunc = JSValue(
          object: { error in
            continuation.resume(
              throwing: JSCWasmError(message: "Promise rejected", value: error)
            )
          } as @convention(block) (JSValue) -> Void,
          in: context
        )
        guard let successFunc, let rejectFunc else {
          continuation.resume(throwing: JSCWasmError(message: "Could not await promise"))
          return
        }
        invokeMethod("then", withArguments: [successFunc, rejectFunc])
        if let exception = context.exception {
          continuation.resume(throwing: JSCWasmError(message: "Promise.then threw", value: exception))
        }
      }
    }
  }
}

#endif

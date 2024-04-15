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

import JavaScriptCore

// returns: (wasm: ArrayBuffer) => Promise<(input: ArrayBuffer) => ArrayBuffer>
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

@available(macOS 10.15, *)
final class JSCWasmPlugin: WasmPlugin {
  private static let factory = JSContext()?.evaluateScript(js)

  private let handler: JSValue

  @MainActor init(wasm data: Data) async throws {
    guard let factory = Self.factory, let context = factory.context else {
      throw JSCWasmError(message: "Failed to load plugin")
    }

    let jsBuf = try JSValue(newBufferWithData: data, in: context)
    guard let promise = factory.call(withArguments: [jsBuf]), context.exception == nil else {
      throw JSCWasmError(message: "Failed to load plugin", value: context.exception)
    }

    handler = try await promise.promiseValue
  }

  @MainActor func handleMessage(_ json: Data) throws -> Data {
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

  fileprivate func arrayBufferData() -> Data {
    let base = JSObjectGetArrayBufferBytesPtr(context.jsGlobalContextRef, jsValueRef, nil)
    let count = JSObjectGetArrayBufferByteLength(context.jsGlobalContextRef, jsValueRef, nil)
    let buf = UnsafeBufferPointer(start: base?.assumingMemoryBound(to: UInt8.self), count: count)
    return Data(buf)
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

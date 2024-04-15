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

// returns: Promise<(ArrayBuffer) => ArrayBuffer>
private let js = """
(async () => {
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
})()
"""

@available(macOS 10.15, *)
final class JSCWasmPlugin: WasmPlugin {
  private let handler: JSValue

  @MainActor init(wasm data: Data) async throws {
    guard let context = JSContext() else { throw PluginServerError(message: "Could not create JSContext") }

    let jsBuf = try JSValue(newBufferWithData: data, in: context)
    context.globalObject.setObject(jsBuf, forKeyedSubscript: "wasmData")

    guard let promise = context.evaluateScript(js) else {
      throw PluginServerError(message: "Failed to load plugin")
    }

    if let error = context.exception {
      throw PluginServerError(message: "Failed to load plugin: \(error)")
    }

    handler = try await promise.promiseValue
  }

  @MainActor func handleMessage(_ json: Data) throws -> Data {
    let jsonJS = try JSValue(newBufferWithData: json, in: handler.context)
    guard let result = handler.call(withArguments: [jsonJS]) else {
      throw PluginServerError(message: "Wasm plugin did not provide a valid response")
    }
    return result.arrayBufferData()
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
              throwing: PluginServerError(message: "\(error)")
            )
          } as @convention(block) (JSValue) -> Void,
          in: context
        )
        guard let successFunc, let rejectFunc else {
          continuation.resume(throwing: PluginServerError(message: "Could not await promise"))
          return
        }
        invokeMethod("then", withArguments: [successFunc, rejectFunc])
        if let exception = context.exception {
          continuation.resume(throwing: PluginServerError(message: "\(exception)"))
        }
      }
    }
  }
}

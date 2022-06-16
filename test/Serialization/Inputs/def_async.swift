public func doSomethingBig() async -> Int { return 0 }

@_alwaysEmitIntoClient
public func doSerializedAsyncLet() async {
  async let _ = ()
}

@_alwaysEmitIntoClient
public func doSerializedAsyncLetTyped() async {
  async let _ : () = ()
}

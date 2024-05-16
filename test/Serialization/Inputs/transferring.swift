
public func testTransferring(_ x: transferring String) -> transferring String { x }
public func testSending(_ x: sending String) -> sending String { x }
public func testTransferringFunc(_ x: (transferring String) -> ()) { fatalError() }
public func testSendingFunc(_ x: (sending String) -> ()) { fatalError() }
public func testTransferringResultFunc(_ x: () -> transferring String) { fatalError() }
public func testSendingResultFunc(_ x: () -> sending String) { fatalError() }

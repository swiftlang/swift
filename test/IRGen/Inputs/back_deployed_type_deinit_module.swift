@backDeployed(before: macOS 99)
@frozen public struct Handle: ~Copyable {
  public var fd: Int
  public init(fd: Int) { self.fd = fd }

  deinit { close(fd) }
}

@inline(never) public func close(_ fd: Int) {}

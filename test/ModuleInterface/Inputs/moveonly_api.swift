
// FIXME: make this test work when this enum is move-only
public enum Descriptor {
  case stdout
  case stderr
  case other(Int)

  public func message() -> String { return "hello world" }
}

public struct File: ~Copyable {

#if SYNTHESIZE_ACCESSORS
  public var fd: Descriptor = .other(1337)
#else
  private var _fd: Descriptor = .other(1337)
  public var fd: Descriptor {
    _modify { yield &_fd }
    _read { yield _fd }
  }
#endif

  public init() {}
}

public class FileHandle {
  #if SYNTHESIZE_ACCESSORS
  public var file: File = File()
  #else
  private var _file: File = File()
  public var file: File {
    _modify { yield &_file }
    _read { yield _file }
  }
  #endif

  #if SYNTHESIZE_ACCESSORS
  public let immutableFile: File = File()
  #else
  public var immutableFile: File {
    return File() // still immutable, but now generated fresh each time!
  }
  #endif

  public init() {}
}

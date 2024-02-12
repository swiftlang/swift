
public class Message { var s: String = "hello" }

@_moveOnly
public struct FileDescriptor {
  public var x: Int = 0
  public var msg: Message = Message()
}

public class FileHandle {
  var file: FileDescriptor = FileDescriptor()
}


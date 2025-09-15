// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name test
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name test
// RUN: %FileCheck --check-prefix INTERFACE %s < %t.swiftinterface

// RUN: %target-swift-frontend -module-name test -emit-silgen %s -o %t.silgen
// RUN: %FileCheck --check-prefix SILGEN %s < %t.silgen

public class Message { var s: String = "hello" }

public struct FileDescriptor: ~Copyable {
  public var x: Int = 0
  public var msg: Message = Message()
}

public class FileHandle {
  // INTERFACE: public var _stored: test.FileDescriptor

  // SILGEN: @_hasStorage @_hasInitialValue public var _stored: FileDescriptor { get set }
  public var _stored: FileDescriptor = FileDescriptor()

  // INTERFACE:       public var file: test.FileDescriptor {
  // INTERFACE-NEXT:    _read
  // INTERFACE-NEXT:    _modify
  // INTERFACE-NEXT:  }

  // SILGEN: public var file: FileDescriptor { _read _modify }
  public var file: FileDescriptor {
    _read { yield _stored }
    _modify { yield &_stored }
  }
}



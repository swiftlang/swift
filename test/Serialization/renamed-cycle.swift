// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module %t/Library.swift -o %t/Library.swiftmodule
// RUN: %target-swift-frontend -emit-module %t/Renamed.swift -o %t/Renamed.swiftmodule -I %t/
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t/

//--- Library.swift

public struct HasRename {
  public init(new: Int) { }
}

//--- Renamed.swift

import Library

extension HasRename {
  @available(*, renamed: "init(new:)")
  public init(old: Int) {
    self.init(new: old)
  }
}

//--- Client.swift

import Library
import Renamed

_ = HasRename(old: 0)

// RUN: %batch-code-completion

public protocol BaseProtocol {}

extension BaseProtocol {
  var indexInParent: Int {
    1
  }
}

public protocol InheritedProtocol: BaseProtocol {}

extension InheritedProtocol {
  #^COMPLETE^#
  // COMPLETE: Decl[InstanceVar]/Super:            var indexInParent: Int; name=indexInParent
}

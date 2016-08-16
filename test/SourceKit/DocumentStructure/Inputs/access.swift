class DefAccess {
  var defProp : Int = 0
  public var pubProp : Int = 0
  private var privProp : Int = 0
  internal func intFunc() {}
  fileprivate func fpFunc() {}
}

public class PubAccess {
  var defProp : Int = 0
  public var pubProp : Int = 0
  private var privProp : Int = 0
  internal func intFunc() {}
  fileprivate func fpFunc() {}

  class Nested {
    func defFunc() {}
  }
}

internal class IntAccess {
  var defProp : Int = 0
  public var pubProp : Int = 0
  private var privProp : Int = 0
  internal func intFunc() {}
  fileprivate func fpFunc() {}
}

private class PrivAccess {
  var defProp : Int = 0
  public var pubProp : Int = 0
  private var privProp : Int = 0
  internal func intFunc() {}
  fileprivate func fpFunc() {}

  class Nested {
    func defFunc() {}
  }
}

func defFunc() {}
public func pubFunc() {}
private func privFunc() {}
internal func intFunc() {}

public enum PubEnum {
  case Some
  case Else
}

private(set) var defPrivSetProp : Int = 0;

public private(set) var pubPrivSetProp : Int {
  get { return 0; }
  set { }
}
public fileprivate(set) var pubFPSetProp : Int {
  get { return 0; }
  set { }
}
public internal(set) var pubIntSetProp : Int {
  get { return 0; }
  set { }
}

public var pubGetOnly : Int {
  get { return 0; }
}

public let pubLetVar : Int

func defProt() {}
public func pubProt() {}
private func privProt() {}
internal func intProt() {}

extension DefAccess {
  func defFunc() {}
}
extension PubAccess {
  func defFunc() {}
}
extension IntAccess {
  func defFunc() {}
}
extension PrivAccess {
  func defFunc() {}
}
private extension PubAccess {
  func privExtFunc() {}
}

typealias defAlias = Int
public typealias pubAlias = Int
private typealias privAlias = Int
internal typealias intAlias = Int

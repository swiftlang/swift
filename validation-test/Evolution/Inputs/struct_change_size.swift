
public struct ChangeSize {
  public init(version: Int32) {
    self._version = T(version)
  }

  public var version: Int32 {
    get { return Int32(_version) }
    set { _version = T(newValue) }
  }

#if BEFORE
  typealias T = Int32
#else
  typealias T = Int64
#endif

  private var _version: T
}

@frozen public struct ChangeFieldOffsetsOfFixedLayout {
  public init(major: Int32, minor: Int32, patch: Int32) {
    self.major = ChangeSize(version: major)
    self.minor = ChangeSize(version: minor)
    self.patch = ChangeSize(version: patch)
  }

  public var major: ChangeSize
  public var minor: ChangeSize
  public var patch: ChangeSize

  public func getVersion() -> String {
    return "\(major.version).\(minor.version).\(patch.version)"
  }
}

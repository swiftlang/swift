
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

@_fixed_layout open class ChangeFieldOffsetsOfFixedLayout {
  public init(major: Int32, minor: Int32, patch: Int32) {
    self.major = ChangeSize(version: major)
    self.minor = ChangeSize(version: minor)
    self.patch = ChangeSize(version: patch)
  }

  open var major: ChangeSize
  open var minor: ChangeSize
  open var patch: ChangeSize

  open func getVersion() -> String {
    return "\(major.version).\(minor.version).\(patch.version)"
  }
}

@_fixed_layout open class ChangeSizeOfSuperclass : ChangeFieldOffsetsOfFixedLayout {
  public init() {
    self.codename = "Big Bang"

    super.init(major: 7, minor: 0, patch: 0)
  }

  open var codename: String

  open override func getVersion() -> String {
    return "\(super.getVersion()) (\(codename))";
  }
}

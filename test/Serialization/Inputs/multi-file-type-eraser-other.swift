@_typeEraser(AnyWindows)
public protocol Vista {
    associatedtype Software : Vista

    var dlls: Software { get }
}

extension Never: Vista {
  public var dlls: Never { fatalError() }
}

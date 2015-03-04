import Foundation

#if FAKE_UIIMAGE
class UIImage : NSObject { }

@objc
protocol ImagePresentingView {
  var hidden: Bool { @objc(isHidden) get set }
}
#endif

extension UIImage : ImagePresentingView { 
  var hidden: Bool {
    @objc(isHidden) get { return true }
    set { }
  }
}

public class SuperA : NSObject {
  public init(foo: Int) {
    super.init()
  }
}

public class SuperB : NSObject {
  public init(foo: String) { super.init() }
  public init(bar: String) { super.init() }
  public convenience init(wibble: String) { self.init(foo: wibble) }
}

public class SubB : SuperB {
  public override init(bar: String) { super.init(bar: bar) }
}

// rdar://problem/19941580
public class Foo : NSObject, FooProto {
  public static var staticEntityName: String = "yo"
  public var entityName: String = "yo"
}

@objc public protocol FooProto {
  static var staticEntityName: String { get }
  var entityName: String { get }
}

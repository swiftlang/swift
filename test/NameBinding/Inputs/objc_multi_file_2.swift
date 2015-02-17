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

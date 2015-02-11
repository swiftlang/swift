import Foundation

extension UIImage : ImagePresentingView { 
  var hidden: Bool {
    @objc(isHidden) get { return true }
    set { }
  }
}

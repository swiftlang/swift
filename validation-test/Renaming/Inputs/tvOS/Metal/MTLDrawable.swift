
@available(tvOS 8.0, *)
protocol MTLDrawable : NSObjectProtocol {
  func present()
  func present(atTime presentationTime: CFTimeInterval)
}

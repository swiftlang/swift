public protocol View {}

struct InternalView : View {}
struct InternalGenericView<T> : View {}

public struct PublicView : View {}
public struct PublicGenericView<T> : View {}

extension View {
  public func passThrough() -> some View {
    return self
  }

  public func wrapWithInternalView() -> some View {
    return InternalView()
  }

  public func wrapWithInternalGenericView() -> some View {
    return InternalGenericView<Self>()
  }

  public func wrapWithPublicView() -> some View {
    return PublicView()
  }

  public func wrapWithPublicGenericView() -> some View {
    return PublicGenericView<Self>()
  }
}

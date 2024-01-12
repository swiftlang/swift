public protocol Reducer<State> {
  associatedtype State
}

public class WindowData {}

public struct CR<State, R: Reducer>: Reducer
where State == R.State {
}

internal struct SidebarContextMenu<WindowState: WindowData>: Reducer {
  typealias State = WindowState
}

public protocol Factory {
  associatedtype X: Reducer<WindowData>
  func build() -> X
}

public struct MyFactory<WindowState: WindowData>: Factory {
  public typealias State = WindowData
  public init() {}
  public func build() -> some Reducer<WindowData> {
    CR<WindowData, SidebarContextMenu<WindowData>>()
  }
}

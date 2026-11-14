import Observation

@available(SwiftStdlib 5.9, *)
@Observable
public final class ObservableSPI {
  public var visible = 0

  @_spi(Testing) public var hidden = 0

  @_spi(Testing)
  @available(SwiftStdlib 5.9, *)
  public var hiddenAndAvailable = 0

  public init() {}
}

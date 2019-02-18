// Input for access-enforcement-wmo multi-file test cases.

var internalGlobal: Int = 0

public var publicGlobal: Int = 0

public class C {
  var setterProp: Int = 0 // Only modified via setter.
  final var finalProp: Int = 0 // modified directly.
  var inlinedProp: Int = 0 // modification via materializeForSet inlined.
  var internalProp: Int = 0 // modified opaquely via materializeForSet.
  var keyPathProp: Int = 0 // modified via a keypath.
  final var finalKeyPathProp: Int = 0 // modified via a keypath.
  public var publicProp: Int = 0
}

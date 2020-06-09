// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name SkipsSPI -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name SkipsSPI -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/SkipsSPI.symbols.json

// CHECK-NOT: ShouldntAppear

@_spi(OtherModule)
public struct StructShouldntAppear {
  // This shouldn't appear because the owner is @_spi(OtherModule).
  public func functionShouldntAppear() {}

  // Although not @_spi(OtherModule), is in @_spi(OtherModule) struct, so shouldn't appear.
  public struct InnerStructShouldntAppear {}
}

@_spi(OtherModule)
public func functionShouldntAppear() {}

@_spi(OtherModule)
public protocol ProtocolShouldntAppear {}

@_spi(OtherModule)
public enum EnumShouldntAppear {}

@_spi(OtherModule)
public class ClassShouldntAppear {}

// This struct should appear
public struct StructShouldAppear {

  // This shouldn't appear beacause it is @_spi(OtherModule), despite `StructShouldAppear`.
  @_spi(OtherModule)
  public func functionShouldntAppear() {}

  // This shouldn't appear beacause it is @_spi(OtherModule), despite `StructShouldAppear`.
  @_spi(OtherModule)
  public struct InnerStructShouldntAppear {}
}

extension StructShouldAppear {
  // This shouldn't appear because it is @_spi(OtherModule), despite `StructShouldAppear`.
  @_spi(OtherModule)
  public func extendedFunctionShouldntAppear() {}
}

extension StructShouldAppear.InnerStructShouldntAppear {

  // This should not appear because `StructShouldAppear.InnerStructShouldntAppear`
  // is @_spi(OtherModule).
  public func extendedFunctionShouldntAppear() {}
}

extension StructShouldntAppear.InnerStructShouldntAppear {
  // This should not appear because `StructShouldntAppear.InnerStructShouldntAppear`
  // is @_spi(OtherModule).
  @_spi(OtherModule)
  public func extendedFunctionShouldntAppear() {}
}

@_spi(OtherModule)
extension StructShouldAppear {
  // Although StructShouldAppear is fair to include, this extension is
  // tagged as SPI, so everything inside it should be considered SPI as well.
  public func functionInSPIExtensionShouldntAppear() {}
}

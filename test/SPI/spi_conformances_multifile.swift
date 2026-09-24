/// Checks for conformances that are implied by a conformance declared in an
/// SPI extension.
///
/// When two conformances imply a conformance to the same base protocol, the one
/// that supersedes the other determines the declaration context that the implied
/// conformance is recorded in, and therefore whether uses of it are restricted
/// to SPI clients. A non-SPI implier has to win whenever the choice would
/// otherwise be arbitrary (rdar://184557488).

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Diagnostics must not depend on the order of the files.
// RUN: %target-swift-frontend -typecheck -verify -module-name Lib %t/a.swift %t/b.swift %t/c.swift
// RUN: %target-swift-frontend -typecheck -verify -module-name Lib %t/a.swift %t/c.swift %t/b.swift

//--- a.swift

public protocol BaseProto {}
public struct Holder<T: BaseProto> {}

public protocol PublicRefines: BaseProto {}
@_spi(S) public protocol SPIRefines: BaseProto {}

/// A conformance that is only reachable through an SPI extension is SPI.
public struct SPIOnly {}
@_spi(S) extension SPIOnly: BaseProto {}

public func publicUseOfSPIOnly(_: Holder<SPIOnly>) {} // expected-error {{cannot use conformance of 'SPIOnly' to 'BaseProto' here; the conformance is declared as SPI}}
@_spi(S) public func spiUseOfSPIOnly(_: Holder<SPIOnly>) {} // OK

/// An explicit SPI conformance supersedes one implied by a public conformance,
/// so the conformance to the base protocol really is SPI.
public struct ExplicitSPI {}
@_spi(S) extension ExplicitSPI: BaseProto {}
extension ExplicitSPI: PublicRefines {} // expected-error {{cannot use conformance of 'ExplicitSPI' to 'BaseProto' here; the conformance is declared as SPI}}

/// Two conformances in this file imply 'BaseProto', one of them from an SPI
/// context. The non-SPI implier wins in either source order.
@_spi(S) extension SPIImpliedFirst: SPIRefines {}
public struct SPIImpliedFirst: PublicRefines {} // OK

public struct SPIImpliedSecond: PublicRefines {}
@_spi(S) extension SPIImpliedSecond: SPIRefines {} // OK

public func publicUseOfImplied(_: Holder<SPIImpliedFirst>, _: Holder<SPIImpliedSecond>) {} // OK

/// Neither implier of 'Conformer: BaseProto' is in this file, so the choice
/// between them would otherwise fall back to the order of the files.
public struct Conformer {}

public func publicUseOfConformer(_: Holder<Conformer>) {} // OK

//--- b.swift

@_spi(S) extension Conformer: SPIRefines {}

/// The only implier of 'SynthesizedConformer: Equatable' in the file that
/// declares the type is this SPI one, so it wins over the non-SPI implier in
/// c.swift; otherwise '==' could not be synthesized here.
public struct SynthesizedConformer {
  public var value: Int
}

@_spi(S) extension SynthesizedConformer: Hashable {}

//--- c.swift

extension Conformer: PublicRefines {}

internal protocol InternalRefinesEquatable: Equatable {}

extension SynthesizedConformer: InternalRefinesEquatable {} // OK

/// FIXME: 'SynthesizedConformer: Equatable' is recorded in the SPI extension in
/// b.swift, since that is the file that declares the type, so a public
/// conformance that implies it is still diagnosed here. Preferring the non-SPI
/// implier instead would break synthesis in b.swift.
public protocol PublicRefinesEquatable: Equatable {}

extension SynthesizedConformer: PublicRefinesEquatable {} // expected-error {{cannot use conformance of 'SynthesizedConformer' to 'Equatable' here; the conformance is declared as SPI}}

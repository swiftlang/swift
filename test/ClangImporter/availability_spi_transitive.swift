// REQUIRES: OS=macosx
// RUN: %target-swift-frontend -typecheck %s -F %S/Inputs/frameworks -verify -verify-ignore-unrelated -library-level api

import SPIContainerImporter

@_spi(a) public let a: SPIInterface1

public let c: SPIInterface1 // expected-error{{cannot use class 'SPIInterface1' in a property declaration marked public or in a '@frozen' or '@usableFromInline' context; it is an SPI imported from 'SPIContainer'}}

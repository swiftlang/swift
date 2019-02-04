//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import Intents
import Foundation

#if os(iOS)
@available(iOS 10.0, *)
extension INSetDefrosterSettingsInCarIntent {
    
    @nonobjc
    @available(iOS 12.0, *)
    public convenience init(
        enable: Bool? = nil, defroster: INCarDefroster = .unknown, carName: INSpeakableString? = nil
        ) {
        self.init(__enable: enable.map { NSNumber(value: $0) },
                  defroster: defroster,
                  carName: carName)
    }
    
    @nonobjc
    @available(iOS, obsoleted: 12.0)
    public convenience init(
        enable: Bool? = nil, defroster: INCarDefroster = .unknown
        ) {
        self.init(__enable: enable.map { NSNumber(value: $0) },
                  defroster: defroster)
    }
    
    @nonobjc
    public final var enable: Bool? {
        return __enable?.boolValue
    }
}
#endif

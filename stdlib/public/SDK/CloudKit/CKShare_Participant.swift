//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import CloudKit // Clang module

@nonobjc
@available(macOS 10.12, iOS 10.0, tvOS 10.0, watchOS 3.0, *)
extension CKShare.Participant {
    /// TODO: replace these with a .apinotes entry once there's a fix for 39261783
    @available(swift 4.2)
    public typealias AcceptanceStatus = CKShare_Participant_AcceptanceStatus

    @available(swift 4.2)
    public typealias Permission = CKShare_Participant_Permission

    @available(swift 4.2)
    @available(macOS 10.14, iOS 12.0, tvOS 12.0, watchOS 5.0, *)
    public typealias Role = CKShare_Participant_Role

    @available(swift 4.2)
    @available(macOS, deprecated: 10.14, renamed: "CKShare.Participant.Role")
    @available(iOS, deprecated: 12.0, renamed: "CKShare.Participant.Role")
    @available(tvOS, deprecated: 12.0, renamed: "CKShare.Participant.Role")
    @available(watchOS, deprecated: 5.0, renamed: "CKShare.Participant.Role")
    public typealias ParticipantType = CKShare_Participant_ParticipantType
}

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
@nonobjc
@available(iOS 10.0, *)
extension INSetProfileInCarIntent {

    @available(iOS 12.0, *)
    public convenience init(profileNumber: Int? = nil, profileName: String? = nil, isDefaultProfile: Bool? = nil, carName: INSpeakableString? = nil) {
        self.init(__profileNumber: profileNumber.map { NSNumber(value: $0) }, profileName: profileName, defaultProfile: isDefaultProfile.map { NSNumber(value: $0) }, carName: carName)
    }

    @available(iOS, introduced: 11.0, obsoleted: 12.0)
    public convenience init(profileNumber: Int? = nil, profileName: String? = nil, isDefaultProfile: Bool? = nil) {
        self.init(__profileNumber: profileNumber.map { NSNumber(value: $0) }, profileName: profileName, defaultProfile: isDefaultProfile.map { NSNumber(value: $0) })
    }

    @available(iOS, deprecated: 11.0, renamed: "init(profileNumber:profileName:isDefaultProfile:)")
    public convenience init(defaultProfile: Int?) {
        if #available(iOS 10.3, *) {
            self.init(__profileNumber: nil, profileName: nil, defaultProfile: defaultProfile.map { NSNumber(value: $0) })
        }
        else {
            self.init(__profileNumber: nil, profileLabel: nil, defaultProfile: defaultProfile.map { NSNumber(value: $0) })
        }
    }

    @available(iOS, deprecated: 11.0, renamed: "init(profileNumber:profileName:isDefaultProfile:)")
    public convenience init(profileLabel: String?) {
        if #available(iOS 10.3, *) {
            self.init(__profileNumber: nil, profileName: profileLabel, defaultProfile: nil)
        }
        else {
            self.init(__profileNumber: nil, profileLabel: profileLabel, defaultProfile: nil)
        }
    }

    @available(iOS, deprecated: 11.0, renamed: "init(profileNumber:profileName:isDefaultProfile:)")
    public convenience init(profileLabel: String?, defaultProfile: Int?) {
        if #available(iOS 10.3, *) {
            self.init(__profileNumber: nil, profileName: profileLabel, defaultProfile: defaultProfile.map { NSNumber(value: $0) })
        }
        else {
            self.init(__profileNumber: nil, profileLabel: profileLabel, defaultProfile: defaultProfile.map { NSNumber(value: $0) })
        }
    }

    @available(iOS, deprecated: 11.0, renamed: "init(profileNumber:profileName:isDefaultProfile:)")
    public convenience init(profileLabel: String?, isDefaultProfile: Bool?) {
        if #available(iOS 10.3, *) {
            self.init(__profileNumber: nil, profileName: profileLabel, defaultProfile: isDefaultProfile.map { NSNumber(value: $0) })
        }
        else {
            self.init(__profileNumber: nil, profileLabel: profileLabel, defaultProfile: isDefaultProfile.map { NSNumber(value: $0) })
        }
    }

    @available(iOS, deprecated: 11.0, renamed: "init(profileNumber:profileName:isDefaultProfile:)")
    public convenience init(profileName: String?, defaultProfile: Int?) {
        if #available(iOS 10.3, *) {
            self.init(__profileNumber: nil, profileName: profileName, defaultProfile: defaultProfile.map { NSNumber(value: $0) })
        }
        else {
            self.init(__profileNumber: nil, profileLabel: profileName, defaultProfile: defaultProfile.map { NSNumber(value: $0) })
        }
    }

    @available(iOS, deprecated: 11.0, renamed: "init(profileNumber:profileName:isDefaultProfile:)")
    public convenience init(profileNumber: Int?, defaultProfile: Int?) {
        if #available(iOS 10.3, *) {
            self.init(__profileNumber: profileNumber.map { NSNumber(value: $0) }, profileName: nil, defaultProfile: defaultProfile.map { NSNumber(value: $0) })
        }
        else {
            self.init(__profileNumber: profileNumber.map { NSNumber(value: $0) }, profileLabel: nil, defaultProfile: defaultProfile.map { NSNumber(value: $0) })
        }
    }

    @available(iOS, deprecated: 11.0, renamed: "init(profileNumber:profileName:isDefaultProfile:)")
    public convenience init(profileNumber: Int?, profileLabel: String?) {
        if #available(iOS 10.3, *) {
            self.init(__profileNumber: profileNumber.map { NSNumber(value: $0) }, profileName: profileLabel, defaultProfile: nil)
        }
        else {
            self.init(__profileNumber: profileNumber.map { NSNumber(value: $0) }, profileLabel: profileLabel, defaultProfile: nil)
        }
    }

    @available(iOS, deprecated: 11.0, renamed: "init(profileNumber:profileName:isDefaultProfile:)")
    public convenience init(profileNumber: Int?, profileLabel: String?, defaultProfile: Int?) {
        if #available(iOS 10.3, *) {
            self.init(__profileNumber: profileNumber.map { NSNumber(value: $0) }, profileName: profileLabel, defaultProfile: defaultProfile.map { NSNumber(value: $0) })
        }
        else {
            self.init(__profileNumber: profileNumber.map { NSNumber(value: $0) }, profileLabel: profileLabel, defaultProfile: defaultProfile.map { NSNumber(value: $0) })
        }
    }

    @available(iOS, deprecated: 11.0, renamed: "init(profileNumber:profileName:isDefaultProfile:)")
    public convenience init(profileNumber: Int?, profileLabel: String?, isDefaultProfile: Bool?) {
        if #available(iOS 10.3, *) {
            self.init(__profileNumber: profileNumber.map { NSNumber(value: $0) }, profileName: profileLabel, defaultProfile: isDefaultProfile.map { NSNumber(value: $0) })
        }
        else {
            self.init(__profileNumber: profileNumber.map { NSNumber(value: $0) }, profileLabel: profileLabel, defaultProfile: isDefaultProfile.map { NSNumber(value: $0) })
        }
    }

    @available(iOS, deprecated: 11.0, renamed: "init(profileNumber:profileName:isDefaultProfile:)")
    public convenience init(profileNumber: Int?, profileName: String?, defaultProfile: Int?) {
        if #available(iOS 10.3, *) {
            self.init(__profileNumber: profileNumber.map { NSNumber(value: $0) }, profileName: profileName, defaultProfile: defaultProfile.map { NSNumber(value: $0) })
        }
        else {
            self.init(__profileNumber: profileNumber.map { NSNumber(value: $0) }, profileLabel: profileName, defaultProfile: defaultProfile.map { NSNumber(value: $0) })
        }
    }

    @available(iOS 10.0, *)
    public var isDefaultProfile: Bool? {
        return __defaultProfile?.boolValue
    }

    @available(swift, deprecated: 3.2, obsoleted: 4.0,
      message: "Please use isDefaultProfile instead")
    public var defaultProfile: Int? {
      return __defaultProfile?.intValue
    }

    @available(iOS 10.0, *)
    public final var profileNumber: Int? {
        return __profileNumber?.intValue
    }
}
#endif

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

#if !os(watchOS)

/**
 The payload of a push notification delivered in the UIApplication `application:didReceiveRemoteNotification:` delegate method contains information about the firing subscription.
 
 Use `Notification(fromRemoteNotificationDictionary:)` to parse that payload.
 On tvOS, alerts, badges, sounds, and categories are not handled in push notifications. However, Subscriptions remain available to help you avoid polling the server.
 */
@nonobjc
@available(macOS 10.10, iOS 8.0, *) @available(watchOS, unavailable)
extension CKSubscription.NotificationInfo {
    /// A list of field names to take from the matching record that is used as substitution variables in a formatted alert string.
    #if !os(tvOS)

    @available(tvOS, unavailable)
    @available(swift 4.2)
    public var alertLocalizationArgs: [CKRecord.FieldKey]? {
        get { return self.__alertLocalizationArgs }
        set { self.__alertLocalizationArgs = newValue }
    }
    
    /// A list of field names to take from the matching record that is used as substitution variables in a formatted title string.
    @available(macOS 10.13, iOS 11.0, *) @available(tvOS, unavailable)
    @available(swift 4.2)
    public var titleLocalizationArgs: [CKRecord.FieldKey]? {
        get { return self.__titleLocalizationArgs }
        set { self.__titleLocalizationArgs = newValue }
    }
    
    /// A list of field names to take from the matching record that is used as substitution variables in a formatted subtitle string.
    @available(macOS 10.13, iOS 11.0, *) @available(tvOS, unavailable)
    @available(swift 4.2)
    public var subtitleLocalizationArgs: [CKRecord.FieldKey]? {
        get { return self.__subtitleLocalizationArgs }
        set { self.__subtitleLocalizationArgs = newValue }
    }
    
    #endif // !os(tvOS)
    
    /**
     A list of keys from the matching record to include in the notification payload.
     
     Only some keys are allowed.  The value types associated with those keys on the server must be one of these classes:
     - CKRecord.Reference
     - CLLocation
     - NSDate
     - NSNumber
     - NSString
     */
    @available(swift 4.2)
    public var desiredKeys: [CKRecord.FieldKey]? {
        get { return self.__desiredKeys }
        set { self.__desiredKeys = newValue }
    }
    
    public convenience init(alertBody: String? = nil,
                            alertLocalizationKey: String? = nil,
                            alertLocalizationArgs: [CKRecord.FieldKey] = [],
                            title: String? = nil,
                            titleLocalizationKey: String? = nil,
                            titleLocalizationArgs: [CKRecord.FieldKey] = [],
                            subtitle: String? = nil,
                            subtitleLocalizationKey: String? = nil,
                            subtitleLocalizationArgs: [CKRecord.FieldKey] = [],
                            alertActionLocalizationKey: String? = nil,
                            alertLaunchImage: String? = nil,
                            soundName: String? = nil,
                            desiredKeys: [CKRecord.FieldKey] = [],
                            shouldBadge: Bool = false,
                            shouldSendContentAvailable: Bool = false,
                            shouldSendMutableContent: Bool = false,
                            category: String? = nil,
                            collapseIDKey: String? = nil) {
        
        self.init()
        
        #if !os(tvOS)
        do {
            self.alertBody = alertBody
            self.alertLocalizationKey = alertLocalizationKey
            self.alertLocalizationArgs = alertLocalizationArgs
        
            if #available(macOS 10.13, iOS 11.0, *) {
                self.title = title
                self.titleLocalizationKey = titleLocalizationKey
                self.titleLocalizationArgs = titleLocalizationArgs
                self.subtitle = subtitle
                self.subtitleLocalizationKey = subtitleLocalizationKey
                self.subtitleLocalizationArgs = subtitleLocalizationArgs
            }
        
            self.alertActionLocalizationKey = alertActionLocalizationKey
            self.alertLaunchImage = alertLaunchImage
            self.soundName = soundName
        }
        #endif // !os(tvOS)
        
        self.desiredKeys = desiredKeys
        
        if #available(tvOS 10.0, *) {
            self.shouldBadge = shouldBadge
        }
        self.shouldSendContentAvailable = shouldSendContentAvailable
        if #available(macOS 10.13, iOS 11.0, tvOS 11.0, *) {
            self.shouldSendMutableContent = shouldSendMutableContent
        }
        #if !os(tvOS)
        do {
            if #available(macOS 10.11, iOS 9.0, *) {
                self.category = category
            }
        }
        #endif // !os(tvOS)
        if #available(macOS 10.13, iOS 11.0, tvOS 11.0, *) {
            self.collapseIDKey = collapseIDKey
        }
    }
}

#endif // !os(watchOS)

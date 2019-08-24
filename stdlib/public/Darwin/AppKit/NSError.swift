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
@_exported import AppKit

extension CocoaError.Code {
  public static var textReadInapplicableDocumentType: CocoaError.Code {
    return CocoaError.Code(rawValue: 65806)
  }
  public static var textWriteInapplicableDocumentType: CocoaError.Code {
    return CocoaError.Code(rawValue: 66062)
  }
  public static var serviceApplicationNotFound: CocoaError.Code {
    return CocoaError.Code(rawValue: 66560)
  }
  public static var serviceApplicationLaunchFailed: CocoaError.Code {
    return CocoaError.Code(rawValue: 66561)
  }
  public static var serviceRequestTimedOut: CocoaError.Code {
    return CocoaError.Code(rawValue: 66562)
  }
  public static var serviceInvalidPasteboardData: CocoaError.Code {
    return CocoaError.Code(rawValue: 66563)
  }
  public static var serviceMalformedServiceDictionary: CocoaError.Code {
    return CocoaError.Code(rawValue: 66564)
  }
  public static var serviceMiscellaneousError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66800)
  }
  public static var sharingServiceNotConfigured: CocoaError.Code {
    return CocoaError.Code(rawValue: 67072)
  }
  @available(macOS 10.13, *)
  public static var fontAssetDownloadError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66304)
  }
}

// Names deprecated late in Swift 3
extension CocoaError.Code {
  @available(*, deprecated, renamed: "textReadInapplicableDocumentType")
  public static var textReadInapplicableDocumentTypeError: CocoaError.Code {
    return CocoaError.Code(rawValue: 65806)
  }
  @available(*, deprecated, renamed: "textWriteInapplicableDocumentType")
  public static var textWriteInapplicableDocumentTypeError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66062)
  }
  @available(*, deprecated, renamed: "serviceApplicationNotFound")
  public static var serviceApplicationNotFoundError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66560)
  }
  @available(*, deprecated, renamed: "serviceApplicationLaunchFailed")
  public static var serviceApplicationLaunchFailedError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66561)
  }
  @available(*, deprecated, renamed: "serviceRequestTimedOut")
  public static var serviceRequestTimedOutError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66562)
  }
  @available(*, deprecated, renamed: "serviceInvalidPasteboardData")
  public static var serviceInvalidPasteboardDataError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66563)
  }
  @available(*, deprecated, renamed: "serviceMalformedServiceDictionary")
  public static var serviceMalformedServiceDictionaryError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66564)
  }
  @available(*, deprecated, renamed: "serviceMiscellaneousError")
  public static var serviceMiscellaneous: CocoaError.Code {
    return CocoaError.Code(rawValue: 66800)
  }
  @available(*, deprecated, renamed: "sharingServiceNotConfigured")
  public static var sharingServiceNotConfiguredError: CocoaError.Code {
    return CocoaError.Code(rawValue: 67072)
  }
}

extension CocoaError {
  public static var textReadInapplicableDocumentType: CocoaError.Code {
    return CocoaError.Code(rawValue: 65806)
  }
  public static var textWriteInapplicableDocumentType: CocoaError.Code {
    return CocoaError.Code(rawValue: 66062)
  }
  public static var serviceApplicationNotFound: CocoaError.Code {
    return CocoaError.Code(rawValue: 66560)
  }
  public static var serviceApplicationLaunchFailed: CocoaError.Code {
    return CocoaError.Code(rawValue: 66561)
  }
  public static var serviceRequestTimedOut: CocoaError.Code {
    return CocoaError.Code(rawValue: 66562)
  }
  public static var serviceInvalidPasteboardData: CocoaError.Code {
    return CocoaError.Code(rawValue: 66563)
  }
  public static var serviceMalformedServiceDictionary: CocoaError.Code {
    return CocoaError.Code(rawValue: 66564)
  }
  public static var serviceMiscellaneous: CocoaError.Code {
    return CocoaError.Code(rawValue: 66800)
  }
  public static var sharingServiceNotConfigured: CocoaError.Code {
    return CocoaError.Code(rawValue: 67072)
  }
  @available(macOS 10.13, *)
  public static var fontAssetDownloadError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66304)
  }
}

// Names deprecated late in Swift 3
extension CocoaError {
  @available(*, deprecated, renamed: "textReadInapplicableDocumentType")
  public static var textReadInapplicableDocumentTypeError: CocoaError.Code {
    return CocoaError.Code(rawValue: 65806)
  }
  @available(*, deprecated, renamed: "textWriteInapplicableDocumentType")
  public static var textWriteInapplicableDocumentTypeError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66062)
  }
  @available(*, deprecated, renamed: "serviceApplicationNotFound")
  public static var serviceApplicationNotFoundError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66560)
  }
  @available(*, deprecated, renamed: "serviceApplicationLaunchFailed")
  public static var serviceApplicationLaunchFailedError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66561)
  }
  @available(*, deprecated, renamed: "serviceRequestTimedOut")
  public static var serviceRequestTimedOutError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66562)
  }
  @available(*, deprecated, renamed: "serviceInvalidPasteboardData")
  public static var serviceInvalidPasteboardDataError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66563)
  }
  @available(*, deprecated, renamed: "serviceMalformedServiceDictionary")
  public static var serviceMalformedServiceDictionaryError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66564)
  }
  @available(*, deprecated, renamed: "serviceMiscellaneous")
  public static var serviceMiscellaneousError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66800)
  }
  @available(*, deprecated, renamed: "sharingServiceNotConfigured")
  public static var sharingServiceNotConfiguredError: CocoaError.Code {
    return CocoaError.Code(rawValue: 67072)
  }
}

extension CocoaError {
  public var isServiceError: Bool {
    return code.rawValue >= 66560 && code.rawValue <= 66817
  }

  public var isSharingServiceError: Bool {
    return code.rawValue >= 67072 && code.rawValue <= 67327
  }

  public var isTextReadWriteError: Bool {
    return code.rawValue >= 65792 && code.rawValue <= 66303
  }

  @available(macOS 10.13, *)
  public var isFontError: Bool {
    return code.rawValue >= 66304 && code.rawValue <= 66335
  }
}

extension CocoaError {
  @available(*, deprecated, renamed: "textReadInapplicableDocumentType")
  public static var TextReadInapplicableDocumentTypeError: CocoaError.Code {
    return CocoaError.Code(rawValue: 65806)
  }
  @available(*, deprecated, renamed: "textWriteInapplicableDocumentType")
  public static var TextWriteInapplicableDocumentTypeError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66062)
  }
  @available(*, deprecated, renamed: "serviceApplicationNotFound")
  public static var ServiceApplicationNotFoundError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66560)
  }
  @available(*, deprecated, renamed: "serviceApplicationLaunchFailed")
  public static var ServiceApplicationLaunchFailedError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66561)
  }
  @available(*, deprecated, renamed: "serviceRequestTimedOut")
  public static var ServiceRequestTimedOutError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66562)
  }
  @available(*, deprecated, renamed: "serviceInvalidPasteboardData")
  public static var ServiceInvalidPasteboardDataError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66563)
  }
  @available(*, deprecated, renamed: "serviceMalformedServiceDictionary")
  public static var ServiceMalformedServiceDictionaryError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66564)
  }
  @available(*, deprecated, renamed: "serviceMiscellaneous")
  public static var ServiceMiscellaneousError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66800)
  }
  @available(*, deprecated, renamed: "sharingServiceNotConfigured")
  public static var SharingServiceNotConfiguredError: CocoaError.Code {
    return CocoaError.Code(rawValue: 67072)
  }
}

extension CocoaError.Code {
  @available(*, deprecated, renamed: "textReadInapplicableDocumentType")
  public static var TextReadInapplicableDocumentTypeError: CocoaError.Code {
    return CocoaError.Code(rawValue: 65806)
  }
  @available(*, deprecated, renamed: "textWriteInapplicableDocumentType")
  public static var TextWriteInapplicableDocumentTypeError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66062)
  }
  @available(*, deprecated, renamed: "serviceApplicationNotFound")
  public static var ServiceApplicationNotFoundError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66560)
  }
  @available(*, deprecated, renamed: "serviceApplicationLaunchFailed")
  public static var ServiceApplicationLaunchFailedError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66561)
  }
  @available(*, deprecated, renamed: "serviceRequestTimedOut")
  public static var ServiceRequestTimedOutError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66562)
  }
  @available(*, deprecated, renamed: "serviceInvalidPasteboardData")
  public static var ServiceInvalidPasteboardDataError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66563)
  }
  @available(*, deprecated, renamed: "serviceMalformedServiceDictionary")
  public static var ServiceMalformedServiceDictionaryError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66564)
  }
  @available(*, deprecated, renamed: "serviceMiscellaneous")
  public static var ServiceMiscellaneousError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66800)
  }
  @available(*, deprecated, renamed: "sharingServiceNotConfigured")
  public static var SharingServiceNotConfiguredError: CocoaError.Code {
    return CocoaError.Code(rawValue: 67072)
  }
}

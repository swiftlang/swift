extension NSCocoaError {
  public static var textReadInapplicableDocumentTypeError: NSCocoaError {
    return NSCocoaError(rawValue: 65806)
  }
  public static var textWriteInapplicableDocumentTypeError: NSCocoaError {
    return NSCocoaError(rawValue: 66062)
  }
  public static var serviceApplicationNotFoundError: NSCocoaError {
    return NSCocoaError(rawValue: 66560)
  }
  public static var serviceApplicationLaunchFailedError: NSCocoaError {
    return NSCocoaError(rawValue: 66561)
  }
  public static var serviceRequestTimedOutError: NSCocoaError {
    return NSCocoaError(rawValue: 66562)
  }
  public static var serviceInvalidPasteboardDataError: NSCocoaError {
    return NSCocoaError(rawValue: 66563)
  }
  public static var serviceMalformedServiceDictionaryError: NSCocoaError {
    return NSCocoaError(rawValue: 66564)
  }
  public static var serviceMiscellaneousError: NSCocoaError {
    return NSCocoaError(rawValue: 66800)
  }
  public static var sharingServiceNotConfiguredError: NSCocoaError {
    return NSCocoaError(rawValue: 67072)
  }

  public var isServiceError: Bool {
    return rawValue >= 66560 && rawValue <= 66817
  }

  public var isSharingServiceError: Bool {
    return rawValue >= 67072 && rawValue <= 67327
  }

  public var isTextReadWriteError: Bool {
    return rawValue >= 65792 && rawValue <= 66303
  }
}

extension NSCocoaError {
  @swift3_migration(renamed: "textReadInapplicableDocumentTypeError")
  public static var TextReadInapplicableDocumentTypeError: NSCocoaError {
    return NSCocoaError(rawValue: 65806)
  }
  @swift3_migration(renamed: "textWriteInapplicableDocumentTypeError")
  public static var TextWriteInapplicableDocumentTypeError: NSCocoaError {
    return NSCocoaError(rawValue: 66062)
  }
  @swift3_migration(renamed: "serviceApplicationNotFoundError")
  public static var ServiceApplicationNotFoundError: NSCocoaError {
    return NSCocoaError(rawValue: 66560)
  }
  @swift3_migration(renamed: "serviceApplicationLaunchFailedError")
  public static var ServiceApplicationLaunchFailedError: NSCocoaError {
    return NSCocoaError(rawValue: 66561)
  }
  @swift3_migration(renamed: "serviceRequestTimedOutError")
  public static var ServiceRequestTimedOutError: NSCocoaError {
    return NSCocoaError(rawValue: 66562)
  }
  @swift3_migration(renamed: "serviceInvalidPasteboardDataError")
  public static var ServiceInvalidPasteboardDataError: NSCocoaError {
    return NSCocoaError(rawValue: 66563)
  }
  @swift3_migration(renamed: "serviceMalformedServiceDictionaryError")
  public static var ServiceMalformedServiceDictionaryError: NSCocoaError {
    return NSCocoaError(rawValue: 66564)
  }
  @swift3_migration(renamed: "serviceMiscellaneousError")
  public static var ServiceMiscellaneousError: NSCocoaError {
    return NSCocoaError(rawValue: 66800)
  }
  @swift3_migration(renamed: "sharingServiceNotConfiguredError")
  public static var SharingServiceNotConfiguredError: NSCocoaError {
    return NSCocoaError(rawValue: 67072)
  }
}

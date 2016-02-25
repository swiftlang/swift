public extension NSCocoaError {
  @swift3_migration(renamed="textReadInapplicableDocumentTypeError")
  public static var TextReadInapplicableDocumentTypeError: NSCocoaError {
    return NSCocoaError(rawValue: 65806)
  }
  @swift3_migration(renamed="textWriteInapplicableDocumentTypeError")
  public static var TextWriteInapplicableDocumentTypeError: NSCocoaError {
    return NSCocoaError(rawValue: 66062)
  }
  @swift3_migration(renamed="serviceApplicationNotFoundError")
  public static var ServiceApplicationNotFoundError: NSCocoaError {
    return NSCocoaError(rawValue: 66560)
  }
  @swift3_migration(renamed="serviceApplicationLaunchFailedError")
  public static var ServiceApplicationLaunchFailedError: NSCocoaError {
    return NSCocoaError(rawValue: 66561)
  }
  @swift3_migration(renamed="serviceRequestTimedOutError")
  public static var ServiceRequestTimedOutError: NSCocoaError {
    return NSCocoaError(rawValue: 66562)
  }
  @swift3_migration(renamed="serviceInvalidPasteboardDataError")
  public static var ServiceInvalidPasteboardDataError: NSCocoaError {
    return NSCocoaError(rawValue: 66563)
  }
  @swift3_migration(renamed="serviceMalformedServiceDictionaryError")
  public static var ServiceMalformedServiceDictionaryError: NSCocoaError {
    return NSCocoaError(rawValue: 66564)
  }
  @swift3_migration(renamed="serviceMiscellaneousError")
  public static var ServiceMiscellaneousError: NSCocoaError {
    return NSCocoaError(rawValue: 66800)
  }
  @swift3_migration(renamed="sharingServiceNotConfiguredError")
  public static var SharingServiceNotConfiguredError: NSCocoaError {
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

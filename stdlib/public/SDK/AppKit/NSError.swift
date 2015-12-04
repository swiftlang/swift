public extension NSCocoaError {
  public static var TextReadInapplicableDocumentTypeError: NSCocoaError {
    return NSCocoaError(rawValue: 65806)
  }
  public static var TextWriteInapplicableDocumentTypeError: NSCocoaError {
    return NSCocoaError(rawValue: 66062)
  }
  public static var ServiceApplicationNotFoundError: NSCocoaError {
    return NSCocoaError(rawValue: 66560)
  }
  public static var ServiceApplicationLaunchFailedError: NSCocoaError {
    return NSCocoaError(rawValue: 66561)
  }
  public static var ServiceRequestTimedOutError: NSCocoaError {
    return NSCocoaError(rawValue: 66562)
  }
  public static var ServiceInvalidPasteboardDataError: NSCocoaError {
    return NSCocoaError(rawValue: 66563)
  }
  public static var ServiceMalformedServiceDictionaryError: NSCocoaError {
    return NSCocoaError(rawValue: 66564)
  }
  public static var ServiceMiscellaneousError: NSCocoaError {
    return NSCocoaError(rawValue: 66800)
  }
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

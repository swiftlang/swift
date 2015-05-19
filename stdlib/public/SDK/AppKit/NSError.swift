public extension NSCocoaError {
  static let TextReadInapplicableDocumentTypeError = NSCocoaError(rawValue: 65806)
  static let TextWriteInapplicableDocumentTypeError = NSCocoaError(rawValue: 66062)
  static let ServiceApplicationNotFoundError = NSCocoaError(rawValue: 66560)
  static let ServiceApplicationLaunchFailedError = NSCocoaError(rawValue: 66561)
  static let ServiceRequestTimedOutError = NSCocoaError(rawValue: 66562)
  static let ServiceInvalidPasteboardDataError = NSCocoaError(rawValue: 66563)
  static let ServiceMalformedServiceDictionaryError = NSCocoaError(rawValue: 66564)
  static let ServiceMiscellaneousError = NSCocoaError(rawValue: 66800)
  static let SharingServiceNotConfiguredError = NSCocoaError(rawValue: 67072)

  public var isServiceError: Bool {
    return rawValue >= 66560 && rawValue <= 66817;
  }

  public var isSharingServiceError: Bool {
    return rawValue >= 67072 && rawValue <= 67327;
  }

  public var isTextReadWriteError: Bool {
    return rawValue >= 65792 && rawValue <= 66303;
  }
}

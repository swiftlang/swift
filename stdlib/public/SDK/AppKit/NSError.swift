extension NSCocoaError.Code {
  public static var textReadInapplicableDocumentTypeError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 65806)
  }
  public static var textWriteInapplicableDocumentTypeError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 66062)
  }
  public static var serviceApplicationNotFoundError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 66560)
  }
  public static var serviceApplicationLaunchFailedError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 66561)
  }
  public static var serviceRequestTimedOutError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 66562)
  }
  public static var serviceInvalidPasteboardDataError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 66563)
  }
  public static var serviceMalformedServiceDictionaryError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 66564)
  }
  public static var serviceMiscellaneousError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 66800)
  }
  public static var sharingServiceNotConfiguredError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 67072)
  }
}

extension NSCocoaError {
  public static var textReadInapplicableDocumentTypeError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 65806)
  }
  public static var textWriteInapplicableDocumentTypeError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 66062)
  }
  public static var serviceApplicationNotFoundError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 66560)
  }
  public static var serviceApplicationLaunchFailedError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 66561)
  }
  public static var serviceRequestTimedOutError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 66562)
  }
  public static var serviceInvalidPasteboardDataError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 66563)
  }
  public static var serviceMalformedServiceDictionaryError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 66564)
  }
  public static var serviceMiscellaneousError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 66800)
  }
  public static var sharingServiceNotConfiguredError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 67072)
  }
}

extension NSCocoaError {
  public var isServiceError: Bool {
    return code.rawValue >= 66560 && code.rawValue <= 66817
  }

  public var isSharingServiceError: Bool {
    return code.rawValue >= 67072 && code.rawValue <= 67327
  }

  public var isTextReadWriteError: Bool {
    return code.rawValue >= 65792 && code.rawValue <= 66303
  }
}

extension NSCocoaError.Code {
  @swift3_migration(renamed: "textReadInapplicableDocumentTypeError")
  public static var TextReadInapplicableDocumentTypeError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 65806)
  }
  @swift3_migration(renamed: "textWriteInapplicableDocumentTypeError")
  public static var TextWriteInapplicableDocumentTypeError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 66062)
  }
  @swift3_migration(renamed: "serviceApplicationNotFoundError")
  public static var ServiceApplicationNotFoundError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 66560)
  }
  @swift3_migration(renamed: "serviceApplicationLaunchFailedError")
  public static var ServiceApplicationLaunchFailedError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 66561)
  }
  @swift3_migration(renamed: "serviceRequestTimedOutError")
  public static var ServiceRequestTimedOutError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 66562)
  }
  @swift3_migration(renamed: "serviceInvalidPasteboardDataError")
  public static var ServiceInvalidPasteboardDataError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 66563)
  }
  @swift3_migration(renamed: "serviceMalformedServiceDictionaryError")
  public static var ServiceMalformedServiceDictionaryError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 66564)
  }
  @swift3_migration(renamed: "serviceMiscellaneousError")
  public static var ServiceMiscellaneousError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 66800)
  }
  @swift3_migration(renamed: "sharingServiceNotConfiguredError")
  public static var SharingServiceNotConfiguredError: NSCocoaError.Code {
    return NSCocoaError.Code(rawValue: 67072)
  }
}

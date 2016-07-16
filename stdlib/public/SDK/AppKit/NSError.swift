extension CocoaError.Code {
  public static var textReadInapplicableDocumentTypeError: CocoaError.Code {
    return CocoaError.Code(rawValue: 65806)
  }
  public static var textWriteInapplicableDocumentTypeError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66062)
  }
  public static var serviceApplicationNotFoundError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66560)
  }
  public static var serviceApplicationLaunchFailedError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66561)
  }
  public static var serviceRequestTimedOutError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66562)
  }
  public static var serviceInvalidPasteboardDataError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66563)
  }
  public static var serviceMalformedServiceDictionaryError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66564)
  }
  public static var serviceMiscellaneousError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66800)
  }
  public static var sharingServiceNotConfiguredError: CocoaError.Code {
    return CocoaError.Code(rawValue: 67072)
  }
}

extension CocoaError {
  public static var textReadInapplicableDocumentTypeError: CocoaError.Code {
    return CocoaError.Code(rawValue: 65806)
  }
  public static var textWriteInapplicableDocumentTypeError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66062)
  }
  public static var serviceApplicationNotFoundError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66560)
  }
  public static var serviceApplicationLaunchFailedError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66561)
  }
  public static var serviceRequestTimedOutError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66562)
  }
  public static var serviceInvalidPasteboardDataError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66563)
  }
  public static var serviceMalformedServiceDictionaryError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66564)
  }
  public static var serviceMiscellaneousError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66800)
  }
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
}

extension CocoaError.Code {
  @swift3_migration(renamed: "textReadInapplicableDocumentTypeError")
  public static var TextReadInapplicableDocumentTypeError: CocoaError.Code {
    return CocoaError.Code(rawValue: 65806)
  }
  @swift3_migration(renamed: "textWriteInapplicableDocumentTypeError")
  public static var TextWriteInapplicableDocumentTypeError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66062)
  }
  @swift3_migration(renamed: "serviceApplicationNotFoundError")
  public static var ServiceApplicationNotFoundError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66560)
  }
  @swift3_migration(renamed: "serviceApplicationLaunchFailedError")
  public static var ServiceApplicationLaunchFailedError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66561)
  }
  @swift3_migration(renamed: "serviceRequestTimedOutError")
  public static var ServiceRequestTimedOutError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66562)
  }
  @swift3_migration(renamed: "serviceInvalidPasteboardDataError")
  public static var ServiceInvalidPasteboardDataError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66563)
  }
  @swift3_migration(renamed: "serviceMalformedServiceDictionaryError")
  public static var ServiceMalformedServiceDictionaryError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66564)
  }
  @swift3_migration(renamed: "serviceMiscellaneousError")
  public static var ServiceMiscellaneousError: CocoaError.Code {
    return CocoaError.Code(rawValue: 66800)
  }
  @swift3_migration(renamed: "sharingServiceNotConfiguredError")
  public static var SharingServiceNotConfiguredError: CocoaError.Code {
    return CocoaError.Code(rawValue: 67072)
  }
}

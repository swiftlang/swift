
let IMAccountSettingServerHost: String
let IMAccountSettingServerPort: String
let IMAccountSettingLoginHandle: String
let IMAccountSettingPassword: String
let IMAccountSettingUsesSSL: String
enum IMSessionAvailability : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case away
  case available
}
let IMSessionPropertyAvailability: String
let IMSessionPropertyStatusMessage: String
let IMSessionPropertyPictureData: String
let IMSessionPropertyIdleDate: String
let IMSessionPropertyIsInvisible: String
enum IMGroupListPermissions : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case canReorderGroup
  case canRenameGroup
  case canAddNewMembers
  case canRemoveMembers
  case canReorderMembers
}
let IMGroupListDefaultGroup: String
let IMGroupListNameKey: String
let IMGroupListPermissionsKey: String
let IMGroupListHandlesKey: String
enum IMHandleAvailability : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unknown
  case offline
  case away
  case available
}
enum IMHandleAuthorizationStatus : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case accepted
  case pending
  case declined
}
let IMHandlePropertyAvailability: String
let IMHandlePropertyStatusMessage: String
let IMHandlePropertyAuthorizationStatus: String
let IMHandlePropertyIdleDate: String
let IMHandlePropertyAlias: String
let IMHandlePropertyFirstName: String
let IMHandlePropertyLastName: String
let IMHandlePropertyEmailAddress: String
let IMHandlePropertyPictureIdentifier: String
let IMHandlePropertyPictureData: String
let IMHandlePropertyCapabilities: String
let IMHandleCapabilityMessaging: String
let IMHandleCapabilityOfflineMessaging: String
let IMHandleCapabilityChatRoom: String
let IMHandleCapabilityHandlePicture: String
let IMHandleCapabilityFileTransfer: String
let IMAttributeFontFamily: String
let IMAttributeFontSize: String
let IMAttributeItalic: String
let IMAttributeBold: String
let IMAttributeUnderline: String
let IMAttributeStrikethrough: String
let IMAttributeLink: String
let IMAttributePreformatted: String
let IMAttributeBaseWritingDirection: String
let IMAttributeForegroundColor: String
let IMAttributeBackgroundColor: String
let IMAttributeMessageBackgroundColor: String

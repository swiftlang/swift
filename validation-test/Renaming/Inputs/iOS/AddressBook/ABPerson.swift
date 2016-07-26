
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use [[CNMutableContact alloc] init]")
@discardableResult
func ABPersonCreate() -> Unmanaged<ABRecord>!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use [[CNMutableContact alloc] init] and [CNSaveRequest addContact:toContainerWithIdentifier:]")
@discardableResult
func ABPersonCreateInSource(_ source: ABRecord!) -> Unmanaged<ABRecord>!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use [CNContactStore containersMatchingPredicate:[CNContainer predicateForContainerOfContactWithIdentifier:] error:]")
@discardableResult
func ABPersonCopySource(_ person: ABRecord!) -> Unmanaged<ABRecord>!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNContactFetchRequest with predicate = [CNContact predicateForContactsLinkedToContact:] and unifyResults = NO")
@discardableResult
func ABPersonCopyArrayOfAllLinkedPeople(_ person: ABRecord!) -> Unmanaged<CFArray>!
@available(iOS, introduced: 2.0, deprecated: 9.0)
@discardableResult
func ABPersonGetTypeOfProperty(_ property: ABPropertyID) -> ABPropertyType
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use [CNContact localizedStringForKey:]")
@discardableResult
func ABPersonCopyLocalizedPropertyName(_ property: ABPropertyID) -> Unmanaged<CFString>!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNContactSortOrder")
typealias ABPersonSortOrdering = UInt32
var kABPersonSortByFirstName: Int { get }
var kABPersonSortByLastName: Int { get }
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use [[CNContactsUserDefaults sharedDefaults] sortOrder]")
@discardableResult
func ABPersonGetSortOrdering() -> ABPersonSortOrdering
@available(iOS, introduced: 2.0, deprecated: 9.0)
typealias ABPersonCompositeNameFormat = UInt32
var kABPersonCompositeNameFormatFirstNameFirst: Int { get }
var kABPersonCompositeNameFormatLastNameFirst: Int { get }
@available(iOS, introduced: 2.0, deprecated: 9.0)
@discardableResult
func ABPersonGetCompositeNameFormat() -> ABPersonCompositeNameFormat
@available(iOS, introduced: 2.0, deprecated: 9.0)
@discardableResult
func ABPersonGetCompositeNameFormatForRecord(_ record: ABRecord!) -> ABPersonCompositeNameFormat
@available(iOS, introduced: 2.0, deprecated: 9.0)
@discardableResult
func ABPersonCopyCompositeNameDelimiterForRecord(_ record: ABRecord!) -> Unmanaged<CFString>!
@available(iOS, introduced: 2.0, deprecated: 9.0)
struct ABPersonImageFormat : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kABPersonImageFormatThumbnail: ABPersonImageFormat { get }
var kABPersonImageFormatOriginalSize: ABPersonImageFormat { get }
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNMutableContact.imageData")
@discardableResult
func ABPersonSetImageData(_ person: ABRecord!, _ imageData: CFData!, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>!) -> Bool
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNContact.imageData")
@discardableResult
func ABPersonCopyImageData(_ person: ABRecord!) -> Unmanaged<CFData>!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNContact.imageData or CNContact.thumbnailImageData")
@discardableResult
func ABPersonCopyImageDataWithFormat(_ person: ABRecord!, _ format: ABPersonImageFormat) -> Unmanaged<CFData>!
@available(iOS, introduced: 2.0, deprecated: 9.0)
@discardableResult
func ABPersonHasImageData(_ person: ABRecord!) -> Bool
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNMutableContact.imageData = nil")
@discardableResult
func ABPersonRemoveImageData(_ person: ABRecord!, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>!) -> Bool
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use [CNContact comparatorForNameSortOrder:]")
@discardableResult
func ABPersonComparePeopleByName(_ person1: ABRecord!, _ person2: ABRecord!, _ ordering: ABPersonSortOrdering) -> CFComparisonResult
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use count of fetch results for CNContactFetchRequest with predicate = nil")
@discardableResult
func ABAddressBookGetPersonCount(_ addressBook: ABAddressBook!) -> CFIndex
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use [CNContactStore unifiedContactWithIdentifier:keysToFetch:error:]")
@discardableResult
func ABAddressBookGetPersonWithRecordID(_ addressBook: ABAddressBook!, _ recordID: ABRecordID) -> Unmanaged<ABRecord>!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNContactFetchRequest with predicate = nil")
@discardableResult
func ABAddressBookCopyArrayOfAllPeople(_ addressBook: ABAddressBook!) -> Unmanaged<CFArray>!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNContactFetchRequest with predicate = [CNContact predicateForContactsInContainerWithIdentifier:] and unifyResults = NO")
@discardableResult
func ABAddressBookCopyArrayOfAllPeopleInSource(_ addressBook: ABAddressBook!, _ source: ABRecord!) -> Unmanaged<CFArray>!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNContactFetchRequest with predicate = [CNContact predicateForContactsInContainerWithIdentifier:] and unifyResults = NO and sortOrder")
@discardableResult
func ABAddressBookCopyArrayOfAllPeopleInSourceWithSortOrdering(_ addressBook: ABAddressBook!, _ source: ABRecord!, _ sortOrdering: ABPersonSortOrdering) -> Unmanaged<CFArray>!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use [CNContactStore unifiedContactsMatchingPredicate:[CNContact predicateForContactsMatchingName:] keysToFetch: error:]")
@discardableResult
func ABAddressBookCopyPeopleWithName(_ addressBook: ABAddressBook!, _ name: CFString!) -> Unmanaged<CFArray>!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use [CNContactVCardSerialization contactsWithData:error:]")
@discardableResult
func ABPersonCreatePeopleInSourceWithVCardRepresentation(_ source: ABRecord!, _ vCardData: CFData!) -> Unmanaged<CFArray>!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use [CNContactVCardSerialization dataWithContacts:error:]")
@discardableResult
func ABPersonCreateVCardRepresentationWithPeople(_ people: CFArray!) -> Unmanaged<CFData>!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNLabelWork")
let kABWorkLabel: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNLabelHome")
let kABHomeLabel: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNLabelOther")
let kABOtherLabel: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNContact.givenName")
let kABPersonFirstNameProperty: ABPropertyID
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNContact.familyName")
let kABPersonLastNameProperty: ABPropertyID
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNContact.middleName")
let kABPersonMiddleNameProperty: ABPropertyID
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNContact.namePrefix")
let kABPersonPrefixProperty: ABPropertyID
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNContact.nameSuffix")
let kABPersonSuffixProperty: ABPropertyID
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNContact.nickname")
let kABPersonNicknameProperty: ABPropertyID
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNContact.phoneticGivenName")
let kABPersonFirstNamePhoneticProperty: ABPropertyID
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNContact.phoneticFamilyName")
let kABPersonLastNamePhoneticProperty: ABPropertyID
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNContact.phoneticMiddleName")
let kABPersonMiddleNamePhoneticProperty: ABPropertyID
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNContact.organizationName")
let kABPersonOrganizationProperty: ABPropertyID
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNContact.departmentName")
let kABPersonDepartmentProperty: ABPropertyID
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNContact.jobTitle")
let kABPersonJobTitleProperty: ABPropertyID
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNContact.emailAddresses")
let kABPersonEmailProperty: ABPropertyID
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNContact.birthday")
let kABPersonBirthdayProperty: ABPropertyID
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNContact.note")
let kABPersonNoteProperty: ABPropertyID
@available(iOS, introduced: 2.0, deprecated: 9.0)
let kABPersonCreationDateProperty: ABPropertyID
@available(iOS, introduced: 2.0, deprecated: 9.0)
let kABPersonModificationDateProperty: ABPropertyID
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNContact.postalAddresses")
let kABPersonAddressProperty: ABPropertyID
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNPostalAddress.street")
let kABPersonAddressStreetKey: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNPostalAddress.city")
let kABPersonAddressCityKey: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNPostalAddress.state")
let kABPersonAddressStateKey: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNPostalAddress.postalCode")
let kABPersonAddressZIPKey: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNPostalAddress.country")
let kABPersonAddressCountryKey: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNPostalAddress.ISOCountryCode")
let kABPersonAddressCountryCodeKey: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNContact.dates")
let kABPersonDateProperty: ABPropertyID
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNLabelDateAnniversary")
let kABPersonAnniversaryLabel: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNContact.contactType")
let kABPersonKindProperty: ABPropertyID
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNContactTypePerson")
let kABPersonKindPerson: CFNumber!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNContactTypeOrganization")
let kABPersonKindOrganization: CFNumber!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNContact.phoneNumbers")
let kABPersonPhoneProperty: ABPropertyID
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNLabelPhoneNumberMobile")
let kABPersonPhoneMobileLabel: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNLabelPhoneNumberiPhone")
let kABPersonPhoneIPhoneLabel: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNLabelPhoneNumberMain")
let kABPersonPhoneMainLabel: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNLabelPhoneNumberHomeFax")
let kABPersonPhoneHomeFAXLabel: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNLabelPhoneNumberWorkFax")
let kABPersonPhoneWorkFAXLabel: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNLabelPhoneNumberOtherFax")
let kABPersonPhoneOtherFAXLabel: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNLabelPhoneNumberPager")
let kABPersonPhonePagerLabel: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNContact.instantMessageAddresses")
let kABPersonInstantMessageProperty: ABPropertyID
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNInstantMessageAddress.service")
let kABPersonInstantMessageServiceKey: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNInstantMessageServiceYahoo")
let kABPersonInstantMessageServiceYahoo: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNInstantMessageServiceJabber")
let kABPersonInstantMessageServiceJabber: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNInstantMessageServiceMSN")
let kABPersonInstantMessageServiceMSN: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNInstantMessageServiceICQ")
let kABPersonInstantMessageServiceICQ: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNInstantMessageServiceAIM")
let kABPersonInstantMessageServiceAIM: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNInstantMessageServiceQQ")
let kABPersonInstantMessageServiceQQ: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNInstantMessageServiceGoogleTalk")
let kABPersonInstantMessageServiceGoogleTalk: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNInstantMessageServiceSkype")
let kABPersonInstantMessageServiceSkype: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNInstantMessageServiceFacebook")
let kABPersonInstantMessageServiceFacebook: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNInstantMessageServiceGaduGadu")
let kABPersonInstantMessageServiceGaduGadu: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNInstantMessageAddress.username")
let kABPersonInstantMessageUsernameKey: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNContact.urlAddresses")
let kABPersonURLProperty: ABPropertyID
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNLabelURLAddressHomePage")
let kABPersonHomePageLabel: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNContact.contactRelations")
let kABPersonRelatedNamesProperty: ABPropertyID
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNLabelContactRelationFather")
let kABPersonFatherLabel: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNLabelContactRelationMother")
let kABPersonMotherLabel: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNLabelContactRelationParent")
let kABPersonParentLabel: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNLabelContactRelationBrother")
let kABPersonBrotherLabel: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNLabelContactRelationSister")
let kABPersonSisterLabel: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNLabelContactRelationChild")
let kABPersonChildLabel: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNLabelContactRelationFriend")
let kABPersonFriendLabel: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNLabelContactRelationSpouse")
let kABPersonSpouseLabel: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNLabelContactRelationPartner")
let kABPersonPartnerLabel: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNLabelContactRelationAssistant")
let kABPersonAssistantLabel: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNLabelContactRelationManager")
let kABPersonManagerLabel: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNContact.socialProfiles")
let kABPersonSocialProfileProperty: ABPropertyID
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNSocialProfile.urlString")
let kABPersonSocialProfileURLKey: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNSocialProfile.service")
let kABPersonSocialProfileServiceKey: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNSocialProfile.username")
let kABPersonSocialProfileUsernameKey: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNSocialProfile.userIdentifier")
let kABPersonSocialProfileUserIdentifierKey: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNSocialProfileServiceTwitter")
let kABPersonSocialProfileServiceTwitter: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNSocialProfileServiceSinaWeibo")
let kABPersonSocialProfileServiceSinaWeibo: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNSocialProfileServiceGameCenter")
let kABPersonSocialProfileServiceGameCenter: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNSocialProfileServiceFacebook")
let kABPersonSocialProfileServiceFacebook: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNSocialProfileServiceMySpace")
let kABPersonSocialProfileServiceMyspace: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNSocialProfileServiceLinkedIn")
let kABPersonSocialProfileServiceLinkedIn: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNSocialProfileServiceFlickr")
let kABPersonSocialProfileServiceFlickr: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use CNContact.nonGregorianBirthday")
let kABPersonAlternateBirthdayProperty: ABPropertyID
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use NSDateComponents.calendar")
let kABPersonAlternateBirthdayCalendarIdentifierKey: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use NSDateComponents.era")
let kABPersonAlternateBirthdayEraKey: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use NSDateComponents.year")
let kABPersonAlternateBirthdayYearKey: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use NSDateComponents.month")
let kABPersonAlternateBirthdayMonthKey: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use NSDateComponents.isLeapMonth")
let kABPersonAlternateBirthdayIsLeapMonthKey: CFString!
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "use NSDateComponents.day")
let kABPersonAlternateBirthdayDayKey: CFString!

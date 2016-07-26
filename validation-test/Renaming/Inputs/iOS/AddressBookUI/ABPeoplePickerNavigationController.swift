
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use CNContactPickerViewController from ContactsUI.framework instead")
class ABPeoplePickerNavigationController : UINavigationController {
  unowned(unsafe) var peoplePickerDelegate: @sil_unmanaged ABPeoplePickerNavigationControllerDelegate?
  var displayedProperties: [NSNumber]?
  var addressBook: ABAddressBook?
  @available(iOS 8.0, *)
  @NSCopying var predicateForEnablingPerson: NSPredicate?
  @available(iOS 8.0, *)
  @NSCopying var predicateForSelectionOfPerson: NSPredicate?
  @available(iOS 8.0, *)
  @NSCopying var predicateForSelectionOfProperty: NSPredicate?
}
@available(iOS 8.0, *)
let ABPersonNamePrefixProperty: String
@available(iOS 8.0, *)
let ABPersonGivenNameProperty: String
@available(iOS 8.0, *)
let ABPersonMiddleNameProperty: String
@available(iOS 8.0, *)
let ABPersonFamilyNameProperty: String
@available(iOS 8.0, *)
let ABPersonNameSuffixProperty: String
@available(iOS 8.0, *)
let ABPersonPreviousFamilyNameProperty: String
@available(iOS 8.0, *)
let ABPersonNicknameProperty: String
@available(iOS 8.0, *)
let ABPersonPhoneticGivenNameProperty: String
@available(iOS 8.0, *)
let ABPersonPhoneticMiddleNameProperty: String
@available(iOS 8.0, *)
let ABPersonPhoneticFamilyNameProperty: String
@available(iOS 8.0, *)
let ABPersonOrganizationNameProperty: String
@available(iOS 8.0, *)
let ABPersonDepartmentNameProperty: String
@available(iOS 8.0, *)
let ABPersonJobTitleProperty: String
@available(iOS 8.0, *)
let ABPersonBirthdayProperty: String
@available(iOS 8.0, *)
let ABPersonNoteProperty: String
@available(iOS 8.0, *)
let ABPersonPhoneNumbersProperty: String
@available(iOS 8.0, *)
let ABPersonEmailAddressesProperty: String
@available(iOS 8.0, *)
let ABPersonUrlAddressesProperty: String
@available(iOS 8.0, *)
let ABPersonDatesProperty: String
@available(iOS 8.0, *)
let ABPersonInstantMessageAddressesProperty: String
@available(iOS 8.0, *)
let ABPersonRelatedNamesProperty: String
@available(iOS 8.0, *)
let ABPersonSocialProfilesProperty: String
@available(iOS 8.0, *)
let ABPersonPostalAddressesProperty: String
protocol ABPeoplePickerNavigationControllerDelegate : NSObjectProtocol {
  @available(iOS 8.0, *)
  optional func peoplePickerNavigationController(_ peoplePicker: ABPeoplePickerNavigationController, didSelectPerson person: ABRecord)
  @available(iOS 8.0, *)
  optional func peoplePickerNavigationController(_ peoplePicker: ABPeoplePickerNavigationController, didSelectPerson person: ABRecord, property property: ABPropertyID, identifier identifier: ABMultiValueIdentifier)
  @available(iOS 2.0, *)
  optional func peoplePickerNavigationControllerDidCancel(_ peoplePicker: ABPeoplePickerNavigationController)
  @available(iOS, introduced: 2.0, deprecated: 8.0)
  @discardableResult
  optional func peoplePickerNavigationController(_ peoplePicker: ABPeoplePickerNavigationController, shouldContinueAfterSelectingPerson person: ABRecord) -> Bool
  @available(iOS, introduced: 2.0, deprecated: 8.0)
  @discardableResult
  optional func peoplePickerNavigationController(_ peoplePicker: ABPeoplePickerNavigationController, shouldContinueAfterSelectingPerson person: ABRecord, property property: ABPropertyID, identifier identifier: ABMultiValueIdentifier) -> Bool
}

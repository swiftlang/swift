
@available(iOS 9.0, *)
class PKContact : NSObject {
  var name: NSPersonNameComponents?
  var postalAddress: CNPostalAddress?
  var emailAddress: String?
  var phoneNumber: CNPhoneNumber?
  @available(iOS 9.2, *)
  var supplementarySubLocality: String?
}

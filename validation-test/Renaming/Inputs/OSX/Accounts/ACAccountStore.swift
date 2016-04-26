
enum ACAccountCredentialRenewResult : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case renewed
  case rejected
  case failed
}
typealias ACAccountStoreSaveCompletionHandler = (Bool, NSError!) -> Void
typealias ACAccountStoreRemoveCompletionHandler = (Bool, NSError!) -> Void
typealias ACAccountStoreRequestAccessCompletionHandler = (Bool, NSError!) -> Void
typealias ACAccountStoreCredentialRenewalHandler = (ACAccountCredentialRenewResult, NSError!) -> Void
@available(OSX 10.8, *)
class ACAccountStore : NSObject {
  var accounts: [AnyObject]! { get }
  @discardableResult
  func account(withIdentifier identifier: String!) -> ACAccount!
  @discardableResult
  func accountType(withAccountTypeIdentifier typeIdentifier: String!) -> ACAccountType!
  @discardableResult
  func accounts(with accountType: ACAccountType!) -> [AnyObject]!
  func saveAccount(_ account: ACAccount!, withCompletionHandler completionHandler: ACAccountStoreSaveCompletionHandler!)
  func requestAccessToAccounts(with accountType: ACAccountType!, options options: [NSObject : AnyObject]! = [:], completion completion: ACAccountStoreRequestAccessCompletionHandler!)
  func renewCredentials(for account: ACAccount!, completion completionHandler: ACAccountStoreCredentialRenewalHandler!)
  func removeAccount(_ account: ACAccount!, withCompletionHandler completionHandler: ACAccountStoreRemoveCompletionHandler!)
}
@available(OSX 10.8, *)
let ACAccountStoreDidChangeNotification: String

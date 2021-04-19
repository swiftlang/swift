import Foundation
import Security

open class SigningIntentProvider: Encodable {

  private enum CodingKeys: String, CodingKey {
    case topic
    case payloadVersion
    case signingIntentKey
    case payload
  }

  let topic: String
  let payloadVersion: Int
  let signingIntentKey: String
  var payload: AnyEncodable

  public let signingKey: SecKey?

  public init(
    topic: String,
    version: Int,
    signingIntentKey: String,
    payload: AnyEncodable
  ) {
    self.topic = topic
    self.payloadVersion = version
    self.signingIntentKey = signingIntentKey
    self.payload = payload
    self.signingKey = nil
  }

  public init(
    topic: String,
    version: Int,
    signingIntentKey: String,
    payload: AnyEncodable,
    signingKey: SecKey
  ) {
    self.topic = topic
    self.payloadVersion = version
    self.signingIntentKey = signingIntentKey
    self.payload = payload
    self.signingKey = signingKey
  }
}

public extension SigningIntentProvider {

  func sign(using privateKey: SecKey?) -> [String: String] {

    let encoder = JSONEncoder()
    encoder.keyEncodingStrategy = .convertToSnakeCase

    do {
      let base64Payload = try encoder.encode(payload).base64EncodedString()
      let signedIntent = SigningIntent(
        topic: topic,
        payloadVersion: payloadVersion,
        base64Payload: base64Payload
      )
      //let jws = JWSTokenFactory.createJWSToken(payload: signedIntent, privateKey: privateKey)
      return [signingIntentKey: "random"]
    } catch {
      fatalError("Encoding intent failed due to \(error.localizedDescription)")
    }
  }
}

private struct SigningIntent: Encodable {
  let topic: String
  let payloadVersion: Int
  let jwsApiVersion = 1 // Defaults to 1 for all signed intents
  let base64Payload: String

  private enum CodingKeys: String, CodingKey {
    case topic
    case payloadVersion
    case jwsApiVersion
    case base64Payload = "payload_b64"
  }
}

struct LoginSigningIntentPayload: Encodable {

  struct Phone: Encodable {
    let countryCode: String
    let nationalNumber: String
  }

  let base64EncryptedPassword: String
  let clientIdempotencyKey: String

  let clientTimestampMs: Int64 = Int64(Date().timeIntervalSince1970 * 1000)
  let appInstallId: String
  let riskPeriodId: String

  private enum CodingKeys: String, CodingKey {
    case base64EncryptedPassword = "encrypted_password_b64"
    case clientIdempotencyKey
    case clientTimestampMs
    case appInstallId = "app_install_uuid"
    case riskPeriodId = "risk_period_uuid"
  }
}

final class LoginSigningIntentProvider: SigningIntentProvider {
  //let topic = "LOGIN"
  //let version = 1
  //let signingIntentKey = "login_signed_intent"

  init(payload: LoginSigningIntentPayload) {
    super.init(
      topic: "LOGIN",
      version: 1,
      signingIntentKey: "login_signed_intent",
      payload: AnyEncodable(payload)
    )
  }
}

let payl = LoginSigningIntentPayload(base64EncryptedPassword: "tP",
   clientIdempotencyKey: "tKey",
   appInstallId: "tId",
   riskPeriodId: "tRisk")
let my_p = LoginSigningIntentProvider(payload: payl)
my_p.sign(using: nil)

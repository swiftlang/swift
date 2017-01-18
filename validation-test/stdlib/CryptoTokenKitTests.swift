// RUN: %target-typecheck-verify-swift

// REQUIRES: objc_interop
// UNSUPPORTED: OS=watchos
// UNSUPPORTED: OS=tvos
// UNSUPPORTED: OS=ios

import CryptoTokenKit
import Security

if #available(OSX 10.12, *) {
  struct TKSmartCardTest {
    func t1(manager: TKSmartCardSlotManager, name: String) {
      manager.getSlot(withName: name) { (slot: TKSmartCardSlot?) in
        let _: TKSmartCardSlot.State = slot!.state
      }
    }

    let p1 = TKSmartCardPINFormat.Charset.numeric
    let p2 = TKSmartCardPINFormat.Encoding.ascii
    let p3 = TKSmartCardPINFormat.Encoding.bcd
    let p4 = TKSmartCardPINFormat.Justification.left
    let p5 = TKSmartCardUserInteractionForPINOperation.Completion.key
    let p6 = TKSmartCardUserInteractionForSecurePINChange.Confirmation.current
    let p7 = TKSmartCardProtocol.t0
    let p8 = TKSmartCardProtocol.t1
    let p9 = TKSmartCardProtocol.t15
    let p10 = TKSmartCardATR.InterfaceGroup()

    func t2(card: TKSmartCard) throws {
      card.isSensitive = card.isValid
      card.transmit(Data()) { (response: Data?, error: Error?) in
      }

      card.userInteractionForSecurePINVerification(TKSmartCardPINFormat(),
          apdu: Data(), pinByteOffset: 0)
      card.userInteractionForSecurePINChange(TKSmartCardPINFormat(),
          apdu: Data(), currentPINByteOffset: 0, newPINByteOffset: 0)

      card.send(ins: 0xa4, p1: 0x04, p2: 0x00, data:Data(), le: 0) {
        (response: Data?, sw: UInt16, error: Error?) in
      }

      card.send(ins: 0xa4, p1: 0x04, p2: 0x00, le: 0) {
        (response: Data?, sw: UInt16, error: Error?) in
      }

      card.send(ins: 0xa4, p1: 0x04, p2: 0x00, data:Data()) {
        (response: Data?, sw: UInt16, error: Error?) in
      }

      card.send(ins: 0xa4, p1: 0x04, p2: 0x00) {
        (response: Data?, sw: UInt16, error: Error?) in
      }

      let _: Int = try card.withSession() {
        let (_, _): (UInt16, Data) = try card.send(ins: 0xa4, p1: 0x04,
            p2: 0x00, data: Data(), le: 0)
        let (_, _): (UInt16, Data) = try card.send(ins: 0xa4, p1: 0x04,
            p2: 0x00, le: 0)
        let (_, _): (UInt16, Data) = try card.send(ins: 0xa4, p1: 0x04,
            p2: 0x00, data: Data())
        let (_, _): (UInt16, Data) = try card.send(ins: 0xa4, p1: 0x04,
            p2: 0x00)
        return 1
      }
    }
  }

  struct TKTokenTest {
    func f1(session: TKTokenSession, sessionDelegate: TKTokenSessionDelegate,
            algorithm: TKTokenKeyAlgorithm,
            parameters: TKTokenKeyExchangeParameters) throws {
      let _: Bool = sessionDelegate.tokenSession!(session, supports: .none,
           keyObjectID: "", algorithm: algorithm)
      let _: Data = try sessionDelegate.tokenSession!(session, sign: Data(),
           keyObjectID: "", algorithm: algorithm)
      let _: Data = try sessionDelegate.tokenSession!(session, decrypt: Data(),
           keyObjectID: "", algorithm: algorithm)
      let _: Data = try sessionDelegate.tokenSession!(session,
           performKeyExchange: Data(), keyObjectID: "", algorithm: algorithm,
           parameters: parameters)
      let _: Bool = algorithm.isAlgorithm(.rsaSignatureDigestPKCS1v15SHA1)
      let _: Bool = algorithm.supportsAlgorithm(.rsaSignatureDigestPKCS1v15SHA1)
    }

    func f2(token: TKToken, delegate: TKTokenDelegate) throws {
      let _: TKTokenSession = try delegate.createSession(token)
    }
  }
}

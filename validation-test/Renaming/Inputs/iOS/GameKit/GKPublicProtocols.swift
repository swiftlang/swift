
protocol GKSessionDelegate : NSObjectProtocol {
  @available(iOS 3.0, *)
  optional func session(_ session: GKSession, peer peerID: String, didChange state: GKPeerConnectionState)
  @available(iOS 3.0, *)
  optional func session(_ session: GKSession, didReceiveConnectionRequestFromPeer peerID: String)
  @available(iOS 3.0, *)
  optional func session(_ session: GKSession, connectionWithPeerFailed peerID: String, withError error: NSError)
  @available(iOS 3.0, *)
  optional func session(_ session: GKSession, didFailWithError error: NSError)
}
protocol GKVoiceChatClient : NSObjectProtocol {
  @available(iOS 3.0, *)
  func voiceChatService(_ voiceChatService: GKVoiceChatService, send data: NSData, toParticipantID participantID: String)
  @discardableResult
  func participantID() -> String
  @available(iOS 3.0, *)
  optional func voiceChatService(_ voiceChatService: GKVoiceChatService, sendRealTime data: NSData, toParticipantID participantID: String)
  @available(iOS 3.0, *)
  optional func voiceChatService(_ voiceChatService: GKVoiceChatService, didStartWithParticipantID participantID: String)
  @available(iOS 3.0, *)
  optional func voiceChatService(_ voiceChatService: GKVoiceChatService, didNotStartWithParticipantID participantID: String, error error: NSError?)
  @available(iOS 3.0, *)
  optional func voiceChatService(_ voiceChatService: GKVoiceChatService, didStopWithParticipantID participantID: String, error error: NSError?)
  @available(iOS 3.0, *)
  optional func voiceChatService(_ voiceChatService: GKVoiceChatService, didReceiveInvitationFromParticipantID participantID: String, callID callID: Int)
}


protocol GKSessionDelegate : NSObjectProtocol {
  @available(tvOS 3.0, *)
  optional func session(_ session: GKSession, peer peerID: String, didChange state: GKPeerConnectionState)
  optional func session(_ session: GKSession, didReceiveConnectionRequestFromPeer peerID: String)
  optional func session(_ session: GKSession, connectionWithPeerFailed peerID: String, withError error: NSError)
  optional func session(_ session: GKSession, didFailWithError error: NSError)
}
protocol GKVoiceChatClient : NSObjectProtocol {
  func voiceChatService(_ voiceChatService: GKVoiceChatService, send data: NSData, toParticipantID participantID: String)
  @discardableResult
  func participantID() -> String
  optional func voiceChatService(_ voiceChatService: GKVoiceChatService, sendRealTime data: NSData, toParticipantID participantID: String)
  optional func voiceChatService(_ voiceChatService: GKVoiceChatService, didStartWithParticipantID participantID: String)
  optional func voiceChatService(_ voiceChatService: GKVoiceChatService, didNotStartWithParticipantID participantID: String, error error: NSError?)
  optional func voiceChatService(_ voiceChatService: GKVoiceChatService, didStopWithParticipantID participantID: String, error error: NSError?)
  optional func voiceChatService(_ voiceChatService: GKVoiceChatService, didReceiveInvitationFromParticipantID participantID: String, callID callID: Int)
}

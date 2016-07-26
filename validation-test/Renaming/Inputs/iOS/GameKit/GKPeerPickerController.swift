
protocol GKPeerPickerControllerDelegate : NSObjectProtocol {
  @available(iOS 3.0, *)
  optional func peerPickerController(_ picker: GKPeerPickerController, didSelect type: GKPeerPickerConnectionType)
  @available(iOS 3.0, *)
  @discardableResult
  optional func peerPickerController(_ picker: GKPeerPickerController, sessionFor type: GKPeerPickerConnectionType) -> GKSession
  @available(iOS 3.0, *)
  optional func peerPickerController(_ picker: GKPeerPickerController, didConnectPeer peerID: String, to session: GKSession)
  @available(iOS 3.0, *)
  optional func peerPickerControllerDidCancel(_ picker: GKPeerPickerController)
}

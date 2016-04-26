
protocol MPPlayableContentDelegate : NSObjectProtocol {
  @available(iOS 7.1, *)
  optional func playableContentManager(_ contentManager: MPPlayableContentManager, initiatePlaybackOfContentItemAt indexPath: NSIndexPath, completionHandler completionHandler: (NSError?) -> Void)
  @available(iOS 9.0, *)
  optional func playableContentManager(_ contentManager: MPPlayableContentManager, initializePlaybackQueueWithCompletionHandler completionHandler: (NSError?) -> Void)
  @available(iOS 8.4, *)
  optional func playableContentManager(_ contentManager: MPPlayableContentManager, didUpdate context: MPPlayableContentManagerContext)
}

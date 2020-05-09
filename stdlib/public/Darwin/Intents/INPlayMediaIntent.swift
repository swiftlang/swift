//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import Intents
import Foundation

#if os(iOS) || os(watchOS)
@available(iOS 12.0, watchOS 5.0, *)
extension INPlayMediaIntent {
  @nonobjc
  public convenience init(
      mediaItems: [INMediaItem]? = nil,
      mediaContainer: INMediaItem? = nil,
      playShuffled: Bool? = nil,
      playbackRepeatMode: INPlaybackRepeatMode = .unknown,
      resumePlayback: Bool? = nil
  ) {
    self.init(__mediaItems: mediaItems,
      mediaContainer: mediaContainer,
      playShuffled: playShuffled.map { NSNumber(value: $0) },
      playbackRepeatMode: playbackRepeatMode,
      resumePlayback: resumePlayback.map { NSNumber(value: $0) })
  }

  @nonobjc
  public final var playShuffled: Bool? {
    return __playShuffled?.boolValue
  }

  @nonobjc
  public final var resumePlayback: Bool? {
    return __resumePlayback?.boolValue
  }
}
#endif

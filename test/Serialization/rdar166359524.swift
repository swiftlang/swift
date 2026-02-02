// RUN: %empty-directory(%t)
// RUN: split-file %s %t --leading-lines

// RUN: %target-swift-frontend -emit-module -o %t -module-name BlockArrayModule %t/BlockArrayModule.swift
// RUN: %target-swift-frontend -typecheck -I %t %t/Client.swift

// No Foundation on other platforms yet.
// REQUIRES: objc_interop
// REQUIRES: VENDOR=apple

//--- BlockArrayModule.swift
import Foundation

@objc public class MediaContent: NSObject {}

public struct MediaContainer {
    public var contentProvider: @convention(block) () -> MediaContent
}

//--- Client.swift
import Foundation
import BlockArrayModule

func process(container: MediaContainer) {
    let _ = container.contentProvider()
}

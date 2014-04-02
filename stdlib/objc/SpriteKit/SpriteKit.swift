@exported import SpriteKit

// SpriteKit defines SKColor using a macro.

#if os(OSX)
typealias SKColor = NSColor
#else
#if os(iOS)
typealias SKColor = UIColor
#endif
#endif


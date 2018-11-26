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

@_exported import AppKit

extension NSEvent {
    public struct SpecialKey : RawRepresentable, Equatable, Hashable {
        public init(rawValue: Int) {
            self.rawValue = rawValue
        }
        public let rawValue: Int
        public var unicodeScalar: Unicode.Scalar {
            return Unicode.Scalar(rawValue)!
        }
    }
    
    /// Returns nil if the receiver is not a "special" key event.
    open var specialKey: SpecialKey? {
        guard let unicodeScalars = charactersIgnoringModifiers?.unicodeScalars else {
            return nil
        }
        guard unicodeScalars.count == 1 else {
            return nil
        }
        guard let codePoint = unicodeScalars.first?.value else {
            return nil
        }
        switch codePoint {
        case 0x0003:
            return .enter
            
        case 0x0008:
            return .backspace
            
        case 0x0009:
            return .tab
            
        case 0x000a:
            return .newline
            
        case 0x000c:
            return .formFeed
            
        case 0x000d:
            return .carriageReturn
            
        case 0x0019:
            return .backTab
            
        case 0x007f:
            return .delete
            
        case 0x2028:
            return .lineSeparator
            
        case 0x2029:
            return .paragraphSeparator
            
        case 0xF700..<0xF900:
            return SpecialKey(rawValue: Int(codePoint))
            
        default:
            return nil
        }
    }
}

extension NSEvent.SpecialKey {
    
    static public let upArrow = NSEvent.SpecialKey(rawValue: 0xF700)
    static public let downArrow = NSEvent.SpecialKey(rawValue: 0xF701)
    static public let leftArrow = NSEvent.SpecialKey(rawValue: 0xF702)
    static public let rightArrow = NSEvent.SpecialKey(rawValue: 0xF703)
    static public let f1 = NSEvent.SpecialKey(rawValue: 0xF704)
    static public let f2 = NSEvent.SpecialKey(rawValue: 0xF705)
    static public let f3 = NSEvent.SpecialKey(rawValue: 0xF706)
    static public let f4 = NSEvent.SpecialKey(rawValue: 0xF707)
    static public let f5 = NSEvent.SpecialKey(rawValue: 0xF708)
    static public let f6 = NSEvent.SpecialKey(rawValue: 0xF709)
    static public let f7 = NSEvent.SpecialKey(rawValue: 0xF70A)
    static public let f8 = NSEvent.SpecialKey(rawValue: 0xF70B)
    static public let f9 = NSEvent.SpecialKey(rawValue: 0xF70C)
    static public let f10 = NSEvent.SpecialKey(rawValue: 0xF70D)
    static public let f11 = NSEvent.SpecialKey(rawValue: 0xF70E)
    static public let f12 = NSEvent.SpecialKey(rawValue: 0xF70F)
    static public let f13 = NSEvent.SpecialKey(rawValue: 0xF710)
    static public let f14 = NSEvent.SpecialKey(rawValue: 0xF711)
    static public let f15 = NSEvent.SpecialKey(rawValue: 0xF712)
    static public let f16 = NSEvent.SpecialKey(rawValue: 0xF713)
    static public let f17 = NSEvent.SpecialKey(rawValue: 0xF714)
    static public let f18 = NSEvent.SpecialKey(rawValue: 0xF715)
    static public let f19 = NSEvent.SpecialKey(rawValue: 0xF716)
    static public let f20 = NSEvent.SpecialKey(rawValue: 0xF717)
    static public let f21 = NSEvent.SpecialKey(rawValue: 0xF718)
    static public let f22 = NSEvent.SpecialKey(rawValue: 0xF719)
    static public let f23 = NSEvent.SpecialKey(rawValue: 0xF71A)
    static public let f24 = NSEvent.SpecialKey(rawValue: 0xF71B)
    static public let f25 = NSEvent.SpecialKey(rawValue: 0xF71C)
    static public let f26 = NSEvent.SpecialKey(rawValue: 0xF71D)
    static public let f27 = NSEvent.SpecialKey(rawValue: 0xF71E)
    static public let f28 = NSEvent.SpecialKey(rawValue: 0xF71F)
    static public let f29 = NSEvent.SpecialKey(rawValue: 0xF720)
    static public let f30 = NSEvent.SpecialKey(rawValue: 0xF721)
    static public let f31 = NSEvent.SpecialKey(rawValue: 0xF722)
    static public let f32 = NSEvent.SpecialKey(rawValue: 0xF723)
    static public let f33 = NSEvent.SpecialKey(rawValue: 0xF724)
    static public let f34 = NSEvent.SpecialKey(rawValue: 0xF725)
    static public let f35 = NSEvent.SpecialKey(rawValue: 0xF726)
    static public let insert = NSEvent.SpecialKey(rawValue: 0xF727)
    static public let deleteForward = NSEvent.SpecialKey(rawValue: 0xF7028)
    static public let home = NSEvent.SpecialKey(rawValue: 0xF729)
    static public let begin = NSEvent.SpecialKey(rawValue: 0xF72A)
    static public let end = NSEvent.SpecialKey(rawValue: 0xF72B)
    static public let pageUp = NSEvent.SpecialKey(rawValue: 0xF72C)
    static public let pageDown = NSEvent.SpecialKey(rawValue: 0xF72D)
    static public let printScreen = NSEvent.SpecialKey(rawValue: 0xF72E)
    static public let scrollLock = NSEvent.SpecialKey(rawValue: 0xF72F)
    static public let pause = NSEvent.SpecialKey(rawValue: 0xF730)
    static public let sysReq = NSEvent.SpecialKey(rawValue: 0xF731)
    static public let `break` = NSEvent.SpecialKey(rawValue: 0xF732)
    static public let reset = NSEvent.SpecialKey(rawValue: 0xF733)
    static public let stop = NSEvent.SpecialKey(rawValue: 0xF734)
    static public let menu = NSEvent.SpecialKey(rawValue: 0xF735)
    static public let user = NSEvent.SpecialKey(rawValue: 0xF736)
    static public let system = NSEvent.SpecialKey(rawValue: 0xF737)
    static public let print = NSEvent.SpecialKey(rawValue: 0xF738)
    static public let clearLine = NSEvent.SpecialKey(rawValue: 0xF739)
    static public let clearDisplay = NSEvent.SpecialKey(rawValue: 0xF73A)
    static public let insertLine = NSEvent.SpecialKey(rawValue: 0xF73B)
    static public let deleteLine = NSEvent.SpecialKey(rawValue: 0xF73C)
    static public let insertCharacter = NSEvent.SpecialKey(rawValue: 0xF73D)
    static public let deleteCharacter = NSEvent.SpecialKey(rawValue: 0xF73E)
    static public let prev = NSEvent.SpecialKey(rawValue: 0xF73F)
    static public let next = NSEvent.SpecialKey(rawValue: 0xF740)
    static public let select = NSEvent.SpecialKey(rawValue: 0xF741)
    static public let execute = NSEvent.SpecialKey(rawValue: 0xF742)
    static public let undo = NSEvent.SpecialKey(rawValue: 0xF743)
    static public let redo = NSEvent.SpecialKey(rawValue: 0xF744)
    static public let find = NSEvent.SpecialKey(rawValue: 0xF745)
    static public let help = NSEvent.SpecialKey(rawValue: 0xF746)
    static public let modeSwitch = NSEvent.SpecialKey(rawValue: 0xF747)
    
    static public let enter = NSEvent.SpecialKey(rawValue: 0x0003)
    static public let backspace = NSEvent.SpecialKey(rawValue: 0x0008)
    static public let tab = NSEvent.SpecialKey(rawValue: 0x0009)
    static public let newline = NSEvent.SpecialKey(rawValue: 0x000a)
    static public let formFeed = NSEvent.SpecialKey(rawValue: 0x000c)
    static public let carriageReturn = NSEvent.SpecialKey(rawValue: 0x000d)
    static public let backTab = NSEvent.SpecialKey(rawValue: 0x0019)
    static public let delete = NSEvent.SpecialKey(rawValue: 0x007f)
    static public let lineSeparator = NSEvent.SpecialKey(rawValue: 0x2028)
    static public let paragraphSeparator = NSEvent.SpecialKey(rawValue: 0x2029)
}

// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: OS=macosx

import Foundation
import CoreGraphics

class Colors {
	static var black = CGColor.black
	static var white = CGColor.white
	static var clear = CGColor.clear

	class func printColors() {
		print("Colors") // CHECK: Colors
		print(black)    // CHECK: Generic Gray
		print(white)    // CHECK: Generic Gray
		print(clear)    // CHECK: Generic Gray
	}
}

// TODO: ColorSpaces with their empty-argument-label pattern, when issue is 
// fixed.

class Events {
	static var mouseDefault = CGEventMouseSubtype.defaultType
	static var mouseTabletPoint = CGEventMouseSubtype.tabletPoint

	static var tapDefault = CGEventTapOptions.defaultTap
	static var tapListenOnly = CGEventTapOptions.listenOnly

	static var privateID = CGEventSourceStateID.privateState
	static var combinedSessionID = CGEventSourceStateID.combinedSessionState

	class func printEvents() {
		print("Events")                   // CHECK-LABEL: Events
	  print(mouseDefault.rawValue)      // CHECK: 0
	  print(mouseTabletPoint.rawValue)  // CHECK: 1
	  print(tapDefault.rawValue)        // CHECK: 0
	  print(tapListenOnly.rawValue)     // CHECK: 1
	  print(privateID.rawValue)         // CHECK: -1
	  print(combinedSessionID.rawValue) // CHECK: 0
	}
}

Colors.printColors()
Events.printEvents()

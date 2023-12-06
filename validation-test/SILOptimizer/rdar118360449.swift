// RUN: %target-swift-frontend %s -sil-verify-all -emit-sil > /dev/null

// REQUIRES: objc_interop

// Check that we don't get two bogus "copy of noncopyable value" errors in the
// timestamp method.

import Foundation

private struct Boop: Hashable {
    let firstName: String
    let lastName: String
    let time: String?
    let source: String?
    let destination: String?
    
    init(firstName: String, lastName: String, time: String?, source: String?, destination: String?) {
        self.firstName = firstName
        self.lastName = lastName
        self.time = time
        self.source = source
        self.destination = destination
    }
}

private struct TripEntry {
    let displayString: String
    let line: Int?
    let column: Int?
    let memberID: String?
    private(set) var timestamp: Boop?
    
    init(_ displayString: String, line: Int? = nil, column: Int? = nil, memberID: String? = nil) {
        self.displayString = displayString
        self.line = line
        self.column = column
        self.memberID = memberID
    }
    
    consuming func timestamp(from lastName: String, time: String? = nil, source: String? = nil, destination: String? = nil) -> TripEntry {
        timestamp = Boop(firstName: displayString.components(separatedBy: " ").first!, lastName: lastName, time: time, source: source, destination: destination)
        return self
    }
}
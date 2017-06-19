// StringUnicode benchmark
//
// rdar://problem/32844779
// rdar://problem/32844807
import Foundation
import TestsUtils

func myTime() -> Double {
    let now = NSDate()
    let seconds = now.timeIntervalSince1970
    let millis = seconds * 1000.0
    return millis
}

@inline(never)
public func run_StringUnicode(N: Int) -> Void {
    let spaces = String(repeating: " ", count: 1000)
    let tenSpaces = String(repeating: " ", count: 10)
    runObjectiveC(spaces as NSString)
    runIterator(spaces)
    runIteratorWithCharacters(spaces)
    runIteratorWithUnicodeScalars(spaces)
    runSubscript(spaces)
    runSubscriptWithHoisting(spaces)
    runSubscriptWithCore(spaces)
    print("")
    runObjectiveC(tenSpaces as NSString)
    runIterator(tenSpaces)
    runIteratorWithCharacters(tenSpaces)
    runIteratorWithUnicodeScalars(tenSpaces)
    runSubscript(tenSpaces)
    runSubscriptWithHoisting(tenSpaces)
    runSubscriptWithCore(tenSpaces)
}

@inline(never) func ignore(_ value: Int) -> Void {
}

@inline(never) func zero() -> Int {
    return 0
}

func runObjectiveC(_ value: NSString) -> Void {
    let n = Int64(value.length)
    let start = myTime()
    var iteration: Int64 = 0
    var sum = 0
    while true {
        for _ in 1...1000 {
            let count = value.length
            for i in 0..<count {
                let c = value.character(at: i)
                sum += Int(c)
            }
        }
        iteration += 1000 * n
        sum = zero()
        let now = myTime()
        if now - start > 5000.0 {
            break
        }
    }
    let timeSecs = (myTime() - start) / 1000.0
    let countPerSec = Int64(Double(iteration) / timeSecs)
    let countPerSecFormatted = String(format: "%ld", locale: Locale.current, countPerSec)
    print("\(type(of: value)).characterAtIndex (\(n) spaces): elapsed time = \(Int(timeSecs * 1000.0)) milliseconds; #characters per second = \(countPerSecFormatted)")
    ignore(sum)
}

func runIterator(_ value: String) -> Void {
    let n = Int64(value.utf16.count)
    let start = myTime()
    var iteration: Int64 = 0
    var sum = 0
    while true {
        for _ in 1...1000 {
            for c in value.utf16 {
                sum += Int(c)
            }
        }
        iteration += 1000 * n
        sum = zero()
        let now = myTime()
        if now - start > 5000.0 {
            break
        }
    }
    let timeSecs = (myTime() - start) / 1000.0
    let countPerSec = Int64(Double(iteration) / timeSecs)
    let countPerSecFormatted = String(format: "%ld", locale: Locale.current, countPerSec)
    print("\(type(of: value)).utf16 (\(n) spaces iterator): elapsed time = \(Int(timeSecs * 1000.0)) milliseconds; #characters per second = \(countPerSecFormatted)")
    ignore(sum)
}

func runIteratorWithCharacters(_ value: String) -> Void {
    let n = Int64(value.utf16.count)
    let start = myTime()
    var iteration: Int64 = 0
    var sum = 0
    while true {
        for _ in 1...1000 {
            for c in value.characters {
                sum += 1 // Int(c)
            }
        }
        iteration += 1000 * n
        sum = zero()
        let now = myTime()
        if now - start > 5000.0 {
            break
        }
    }
    let timeSecs = (myTime() - start) / 1000.0
    let countPerSec = Int64(Double(iteration) / timeSecs)
    let countPerSecFormatted = String(format: "%ld", locale: Locale.current, countPerSec)
    print("\(type(of: value)).characters (\(n) spaces iterator): elapsed time = \(Int(timeSecs * 1000.0)) milliseconds; #characters per second = \(countPerSecFormatted)")
    ignore(sum)
}

func runIteratorWithUnicodeScalars(_ value: String) -> Void {
    let n = Int64(value.utf16.count)
    let start = myTime()
    var iteration: Int64 = 0
    var sum = 0
    while true {
        for _ in 1...1000 {
            for c in value.unicodeScalars {
                sum += Int(c.value)
            }
        }
        iteration += 1000 * n
        sum = zero()
        let now = myTime()
        if now - start > 5000.0 {
            break
        }
    }
    let timeSecs = (myTime() - start) / 1000.0
    let countPerSec = Int64(Double(iteration) / timeSecs)
    let countPerSecFormatted = String(format: "%ld", locale: Locale.current, countPerSec)
    print("\(type(of: value)).unicodeScalars (\(n) spaces iterator): elapsed time = \(Int(timeSecs * 1000.0)) milliseconds; #characters per second = \(countPerSecFormatted)")
    ignore(sum)
}

func runSubscript(_ value: String) -> Void {
    let n = Int64(value.utf16.count)
    let start = myTime()
    var iteration: Int64 = 0
    var sum = 0
    while true {
        for _ in 1...1000 {
            let count = value.utf16.count
            for i in 0..<count {
                let chars = value.utf16
                let index = chars.startIndex
                let c = chars[index.advanced(by: i)]
                sum += Int(c)
            }
        }
        iteration += 1000 * n
        sum = zero()
        let now = myTime()
        if now - start > 5000.0 {
            break
        }
    }
    let timeSecs = (myTime() - start) / 1000.0
    let countPerSec = Int64(Double(iteration) / timeSecs)
    let countPerSecFormatted = String(format: "%ld", locale: Locale.current, countPerSec)
    print("\(type(of: value)).utf16 (\(n) spaces subscript): elapsed time = \(Int(timeSecs * 1000.0)) milliseconds; #characters per second = \(countPerSecFormatted)")
    ignore(sum)
}

func runSubscriptWithHoisting(_ value: String) -> Void {
    let n = Int64(value.utf16.count)
    let start = myTime()
    var iteration: Int64 = 0
    var sum = 0
    while true {
        for _ in 1...1000 {
            let chars = value.utf16
            let count = chars.count
            let index = chars.startIndex
            for i in 0..<count {
                let c = chars[index.advanced(by: i)]
                sum += Int(c)
            }
        }
        iteration += 1000 * n
        sum = zero()
        let now = myTime()
        if now - start > 5000.0 {
            break
        }
    }
    let timeSecs = (myTime() - start) / 1000.0
    let countPerSec = Int64(Double(iteration) / timeSecs)
    let countPerSecFormatted = String(format: "%ld", locale: Locale.current, countPerSec)
    print("\(type(of: value)).utf16 (\(n) spaces subscript - hoisting optimization): elapsed time = \(Int(timeSecs * 1000.0)) milliseconds; #characters per second = \(countPerSecFormatted)")
    ignore(sum)
}

func runSubscriptWithCore(_ value: String) -> Void {
    let n = Int64(value.utf16.count)
    let start = myTime()
    var iteration: Int64 = 0
    var sum = 0
    while true {
        for _ in 1...1000 {
            let count = value._core.count
            for i in 0..<count {
                let c = value._core[i];
                sum += Int(c);
            }
        }
        iteration += 1000 * n
        sum = zero()
        let now = myTime()
        if now - start > 5000.0 {
            break
        }
    }
    let timeSecs = (myTime() - start) / 1000.0
    let countPerSec = Int64(Double(iteration) / timeSecs)
    let countPerSecFormatted = String(format: "%ld", locale: Locale.current, countPerSec)
    print("\(type(of: value))._core (\(n) spaces subscript): elapsed time = \(Int(timeSecs * 1000.0)) milliseconds; #characters per second = \(countPerSecFormatted)")
    ignore(sum)
}


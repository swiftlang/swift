// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: OS=ios

import MediaPlayer
import StdlibUnittest
import StdlibUnittestFoundationExtras

let MediaPlayerTests = TestSuite("MediaPlayer")

MediaPlayerTests.test("decodablePlayParameters") {
    if #available(iOS 11.0, *) {
        let identifier = "1234567890"
        let kind = "song"
        let isLibrary = true
        let playParamsData = """
{
    "id": "\(identifier)",
    "kind": "\(kind)",
    "isLibrary": \(isLibrary)
}
""".data(using: .utf8)!

        do {
            let decoder = JSONDecoder()
            let playParameters = try decoder.decode(MPMusicPlayerPlayParameters.self, from: playParamsData)
            let playParametersDictionary = playParameters.dictionary
            expectEqual(identifier, playParametersDictionary["id"] as! String)
            expectEqual(kind, playParametersDictionary["kind"] as! String)
            expectEqual(isLibrary, playParametersDictionary["isLibrary"] as! Bool)
        }
        catch {
            expectUnreachableCatch(error)
        }
    }
}

MediaPlayerTests.test("decodingInvalidPlayParameters") {
    if #available(iOS 11.0, *) {
        let invalidPlayParamsData = """
{
    "kind": "song"
}
""".data(using: .utf8)!

        do {
            let decoder = JSONDecoder()
            let _ = try decoder.decode(MPMusicPlayerPlayParameters.self, from: invalidPlayParamsData)
            expectUnreachable()
        }
        catch DecodingError.dataCorrupted(_) {}
        catch {
            expectUnreachableCatch(error)
        }
    }
}


MediaPlayerTests.test("encodablePlayParameters") {
    if #available(iOS 11.0, *) {
        let identifier = "1234567890"
        let kind = "song"
        let isLibrary = true
        let correspondingPlayParamsString = """
{"id":"\(identifier)","kind":"\(kind)","isLibrary":\(isLibrary)}
"""

        let playParametersDictionary: [String: Any] = [
            "id": identifier,
            "kind": kind,
            "isLibrary": isLibrary
        ]
        guard let playParameters = MPMusicPlayerPlayParameters(dictionary: playParametersDictionary) else {
            expectUnreachable()
            return
        }

        do {
            let encoder = JSONEncoder()
            let encodedPlayParamsData = try encoder.encode(playParameters)
            let encodedPlayParamsString = String(data: encodedPlayParamsData, encoding: .utf8)!
            expectEqual(correspondingPlayParamsString, encodedPlayParamsString)
        }
        catch {
            expectUnreachableCatch(error)
        }
    }
}

runAllTests()

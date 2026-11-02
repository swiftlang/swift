#if os(Windows)

import CRT
import WinSDK

/// Returns as many whole lines of `log` as fit in an event message.
private func truncated(_ log: String, to maxLen: Int) -> String {
  if log.utf16.count <= maxLen {
    return log
  }

  let truncationNote =
    "\r\n...\r\n(truncated; the full crash log is attached as event data)"

  let limit = log.utf16.index(
    log.startIndex, offsetBy: maxLen - truncationNote.utf16.count)
  let end = log[..<limit].lastIndex(of: "\r\n") ?? log.startIndex
  return String(log[..<end]) + truncationNote
}

// ReportEventW() limits each insertion string to 31,839 UTF-16 code units,
// so the message holds as much of the crash log as fits, and the whole log
// is attached as UTF-8 event data.
private let maxMessageLength = 31_839

internal var backtracerEventLog: EventLogHelper? = {
  EventLogHelper(source: "Swift Backtrace")
}()

internal class EventLogHelper {
  let eventLog: HANDLE

  init?(source: String) {
    let hEventLog = source.withCString(encodedAs: UTF16.self) {
      RegisterEventSourceW(nil, $0)
    }

    guard let hEventLog else {
      return nil
    }

    self.eventLog = hEventLog
  }

  deinit {
    _ = DeregisterEventSource(eventLog)
  }

  internal func report(_ text: String) -> Bool {
    let log = text.split(separator: "\n", omittingEmptySubsequences: false)
      .joined(separator: "\r\n")

    var data = Array(log.utf8)

    return truncated(log, to: maxMessageLength).withCString(encodedAs: UTF16.self) {
      var string: LPCWSTR? = $0
      return ReportEventW(eventLog, EVENTLOG_ERROR_TYPE, 0, 1, nil,
                          1, DWORD(data.count), &string, &data)
    }
  }
}

#endif

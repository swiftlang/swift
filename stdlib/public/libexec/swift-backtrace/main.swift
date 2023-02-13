//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#if os(macOS)

#if canImport(Darwin)
import Darwin.C
#elseif canImport(Glibc)
import Glibc
#elseif canImport(MSVCRT)
import MSVCRT
#endif

@_spi(Formatting) import _Backtracing
@_spi(Contexts) import _Backtracing
@_spi(Registers) import _Backtracing
@_spi(MemoryReaders) import _Backtracing

@main
internal struct SwiftBacktrace {
  enum UnwindAlgorithm {
    case fast
    case precise
  }

  enum Preset {
    case none
    case friendly
    case medium
    case full
  }

  enum ImagesToShow {
    case none
    case all
    case mentioned
  }

  enum RegistersToShow {
    case none
    case all
    case crashedOnly
  }

  struct Arguments {
    var unwindAlgorithm: UnwindAlgorithm = .precise
    var demangle = false
    var interactive = false
    var color = false
    var timeout = 30
    var preset: Preset = .none
    var threads: Bool? = nil
    var registers: RegistersToShow? = nil
    var crashInfo: UInt64? = nil
    var showImages: ImagesToShow? = nil
    var limit: Int? = 64
    var top = 16
    var sanitize: Bool? = nil
  }

  static var args = Arguments()
  static var formattingOptions = BacktraceFormattingOptions()

  static var target: Target? = nil
  static var currentThread: Int = 0

  static var theme: any Theme {
    if args.color {
      return Themes.color
    } else {
      return Themes.plain
    }
  }

  static func usage() {
    print("""
usage: swift-backtrace [--unwind <algorithm>] [--demangle [<bool>]] [--interactive [<bool>]] [--color [<bool>]] [--timeout <seconds>] [--preset <preset>] [--threads [<bool>]] [--registers <registers>] [--images <images>] --crashinfo <addr>

Generate a backtrace for the parent process.

--unwind <algorithm>
-u <algorithm>          Set the unwind algorithm to use.  Supported algorithms
                        are "fast" and "precise".

--demangle [<bool>]
-d [<bool>]             Set whether or not to demangle identifiers.

--interactive [<bool>]
-i [<bool>]             Set whether to be interactive.

--color [<bool>]
-c [<bool>]             Set whether to use ANSI color in the output.

--timeout <seconds>
-t <seconds>            Set how long to wait for interaction.

--preset <preset>
-p <preset>             Set the backtrace format (by preset).  Options are
                        "friendly", "medium" and "full".

--threads [<bool>]
-h [<bool>]             Set whether or not to show all threads.

--registers <registers>
-r <registers>          Set which registers dumps to show.  Options are "none",
                        "all" and "crashed".

--images <images>
-m <images>             Set which images to list.  Options are "none", "all"
                        and "mentioned".

--limit <count>
-l <count>              Set the limit on the number of frames to capture.
                        Can be set to "none" to disable the limit.

--top <count>
-T <count>              Set the minimum number of frames to capture at the top
                        of the stack.  This is used with limit to ensure that
                        you capture sufficient frames to understand deep traces.

--sanitize [<bool>]
-s [<bool>]             Set whether or not to sanitize paths.

--crashinfo <addr>
-a <addr>               Provide a pointer to a platform specific CrashInfo
                        structure.  <addr> should be in hexadecimal.
""")
  }

  static func parseBool(_ s: some StringProtocol) -> Bool {
    let lowered = s.lowercased()
    return lowered == "yes" || lowered == "y" ||
      lowered == "true" || lowered == "t" || lowered == "on"
  }

  static func handleArgument(_ arg: String, value: String?) {
    switch arg {
      case "-?", "--help":
        usage()
        exit(0)
      case "-u", "--unwind":
        if let v = value {
          switch v.lowercased() {
            case "fast":
              args.unwindAlgorithm = .fast
            case "precise":
              args.unwindAlgorithm = .precise
            default:
              print("swift-backtrace: unknown unwind algorithm '\(v)'")
              usage()
              exit(1)
          }
        } else {
          print("swift-backtrace: missing unwind algorithm")
          usage()
          exit(1)
        }
      case "-d", "--demangle":
        if let v = value {
          args.demangle = parseBool(v)
        } else {
          args.demangle = true
        }
      case "-i", "--interactive":
        if let v = value {
          args.interactive = parseBool(v)
        } else {
          args.interactive = true
        }
      case "-c", "--color":
        if let v = value {
          args.color = parseBool(v)
        } else {
          args.color = true
        }
      case "-t", "--timeout":
        if let v = value {
          if let secs = Int(v), secs >= 0 {
            args.timeout = secs
          } else {
            print("swift-backtrace: bad timeout '\(v)'")
          }
        } else {
          print("swift-backtrace: missing timeout value")
          usage()
          exit(1)
        }
      case "-p", "--preset":
        if let v = value {
          switch v.lowercased() {
            case "friendly":
              args.preset = .friendly
            case "medium":
              args.preset = .medium
            case "full":
              args.preset = .full
            default:
              print("swift-backtrace: unknown preset '\(v)'")
              usage()
              exit(1)
          }
        } else {
          print("swift-backtrace: missing preset name")
          usage()
          exit(1)
        }
      case "-h", "--threads":
        if let v = value {
          if v.lowercased() != "preset" {
            args.threads = parseBool(v)
          }
        } else {
          args.threads = true
        }
      case "-r", "--registers":
        if let v = value {
          switch v.lowercased() {
            case "preset":
              break
            case "none":
              args.registers = RegistersToShow.none
            case "all":
              args.registers = .all
            case "crashed":
              args.registers = .crashedOnly
            default:
              print("swift-backtrace: unknown registers setting '\(v)'")
              usage()
              exit(1)
          }
        } else {
          print("swift-backtrace: missing registers setting")
          usage()
          exit(1)
        }
      case "-m", "--images":
        if let v = value {
          switch v.lowercased() {
            case "preset":
              break
            case "none":
              args.showImages = ImagesToShow.none
            case "all":
              args.showImages = .all
            case "mentioned":
              args.showImages = .mentioned
            default:
              print("swift-backtrace: unknown images setting '\(v)'")
              usage()
              exit(1)
          }
        } else {
          print("swift-backtrace: missing images setting")
          usage()
          exit(1)
        }
      case "-l", "--limit":
        if let v = value {
          if v.lowercased() == "none" {
            args.limit = nil
          } else if let limit = Int(v), limit > 0 {
            args.limit = limit
          } else {
            print("swift-backtrace: bad limit value \(v)")
          }
        } else {
          print("swift-backtrace: missing limit value")
          usage()
          exit(1)
        }
      case "-T", "--top":
        if let v = value {
          if let top = Int(v), top >= 0 {
            args.top = top
          } else {
            print("swift-backtrace: bad top value \(v)")
          }
        } else {
          print("swift-backtrace: missing top value")
          usage()
          exit(1)
        }
      case "-s", "--sanitize":
        if let v = value {
          args.sanitize = parseBool(v)
        } else {
          args.sanitize = true
        }
      case "-a", "--crashinfo":
        if let v = value {
          if let a = UInt64(v, radix: 16) {
            args.crashInfo = a
          } else {
            print("swift-backtrace: bad pointer '\(v)'")
            usage()
            exit(1)
          }
        } else {
          print("swift-backtrace: missing pointer value")
          usage()
          exit(1)
        }
      default:
        print("swift-backtrace: unknown argument '\(arg)'")
        usage()
        exit(1)
    }
  }

  static func main() {
    parseArguments()

    guard let crashInfoAddr = args.crashInfo else {
      print("swift-backtrace: --crashinfo is not optional")
      usage()
      exit(1)
    }

    // Set-up the backtrace formatting options
    switch args.preset {
      case .friendly, .none:
        formattingOptions =
          .skipRuntimeFailures(true)
          .showAddresses(false)
          .showSourceCode(true)
          .showFrameAttributes(false)
          .sanitizePaths(args.sanitize ?? false)
        if args.threads == nil {
          args.threads = false
        }
        if args.registers == nil {
          args.registers = RegistersToShow.none
        }
        if args.showImages == nil {
          args.showImages = ImagesToShow.none
        }
      case .medium:
        formattingOptions =
          .skipRuntimeFailures(true)
          .showSourceCode(true)
          .showFrameAttributes(true)
          .sanitizePaths(args.sanitize ?? true)
        if args.threads == nil {
          args.threads = false
        }
        if args.registers == nil {
          args.registers = .crashedOnly
        }
        if args.showImages == nil {
          args.showImages = .mentioned
        }
      case .full:
        formattingOptions =
          .skipRuntimeFailures(false)
          .skipThunkFunctions(false)
          .skipSystemFrames(false)
          .sanitizePaths(args.sanitize ?? true)
        if args.threads == nil {
          args.threads = true
        }
        if args.registers == nil {
          args.registers = .crashedOnly
        }
        if args.showImages == nil {
          args.showImages = .mentioned
        }
    }
    formattingOptions = formattingOptions.demangle(args.demangle)

    // We never use the showImages option; if we're going to show images, we
    // want to do it *once* for all the backtraces we showed.
    formattingOptions = formattingOptions.showImages(.none)

    target = Target(crashInfoAddr: crashInfoAddr,
                    limit: args.limit, top: args.top)

    currentThread = target!.crashingThreadNdx

    printCrashLog()

    print("")

    if args.interactive {
      // Make sure we're line buffered
      setvbuf(stdout, nil, _IOLBF, 0)

      while let ch = waitForKey("Press space to interact, D to debug, or any other key to quit",
                                timeout: args.timeout) {
        switch UInt8(ch) {
          case UInt8(ascii: " "):
            interactWithUser()
            exit(0)
          case UInt8(ascii: "D"), UInt8(ascii: "d"):
            startDebugger()
          default:
            exit(0)
        }
      }
    }

  }

  // Parse the command line arguments; we can't use swift-argument-parser
  // from here because that would create a dependency problem, so we do
  // it manually.
  static func parseArguments() {
    var currentArg: String? = nil
    for arg in CommandLine.arguments[1...] {
      if arg.hasPrefix("-") {
        if let key = currentArg {
          handleArgument(key, value: nil)
        }
        currentArg = arg
      } else {
        if let key = currentArg {
          handleArgument(key, value: arg)
          currentArg = nil
        } else if arg != "" {
          print("swift-backtrace: unexpected argument '\(arg)'")
          usage()
          exit(1)
        }
      }
    }
    if let key = currentArg {
      handleArgument(key, value: nil)
    }
  }

  #if os(Linux) || os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
  static func setRawMode() -> termios {
    var oldAttrs = termios()
    tcgetattr(0, &oldAttrs)

    var newAttrs = oldAttrs
    newAttrs.c_lflag &= ~(UInt(ICANON) | UInt(ECHO))
    tcsetattr(0, TCSANOW, &newAttrs)

    return oldAttrs
  }

  static func resetInputMode(mode: termios) {
    var theMode = mode
    tcsetattr(0, TCSANOW, &theMode)
  }

  static func waitForKey(_ message: String, timeout: Int?) -> Int32? {
    let oldMode = setRawMode()

    defer {
      print("\r\u{1b}[0K", terminator: "")
      fflush(stdout)
      resetInputMode(mode: oldMode)
    }

    if let timeout = timeout {
    var remaining = timeout

    while true {
      print("\r\(message) (\(remaining)s) ", terminator: "")
      fflush(stdout)

      var pfd = pollfd(fd: 0, events: Int16(POLLIN), revents: 0)

      let ret = poll(&pfd, 1, 1000)
      if ret == 0 {
        remaining -= 1
        if remaining == 0 {
          break
        }
        continue
      } else if ret < 0 {
        break
      }

      return getchar()
    }
    } else {
      print("\r\(message)", terminator: "")
      fflush(stdout)
      return getchar()
    }

    return nil
  }
  #elseif os(Windows)
  static func waitForKey(_ message: String, timeout: Int?) -> Int32? {
    // ###TODO
    return nil
  }
  #endif

  static func backtraceFormatter() -> BacktraceFormatter {
    var terminalSize = winsize(ws_row: 24, ws_col: 80,
                               ws_xpixel: 1024, ws_ypixel: 768)
    _ = ioctl(0, TIOCGWINSZ, &terminalSize)

    return BacktraceFormatter(formattingOptions
                              .theme(theme)
                              .width(Int(terminalSize.ws_col)))
  }

  static func printCrashLog() {
    guard let target = target else {
      print("swift-backtrace: unable to get target")
      return
    }

    let crashingThread = target.threads[target.crashingThreadNdx]

    let description: String

    if let failure = crashingThread.backtrace.swiftRuntimeFailure {
      description = failure
    } else {
      description = "Program crashed: \(target.signalDescription) at \(hex(target.faultAddress))"
    }

    print("")
    print(theme.crashReason(description))
    print("")

    var mentionedImages = Set<Int>()
    let formatter = backtraceFormatter()

    func dump(ndx: Int, thread: TargetThread) {
      let crashed = thread.id == target.crashingThread ? " crashed" : ""
      let name = !thread.name.isEmpty ? " \"\(thread.name)\"" : ""
      print("Thread \(ndx)\(name)\(crashed):\n")

      if args.registers! == .all {
        if let context = thread.context {
          showRegisters(context)
        } else {
          print("  " + theme.info("no context for thread \(ndx)"))
        }
        print("")
      }

      let formatted = formatter.format(backtrace: thread.backtrace)

      print(formatted)

      if args.showImages! == .mentioned {
        for frame in thread.backtrace.frames {
          if formatter.shouldSkip(frame) {
            continue
          }
          if let symbol = frame.symbol, symbol.imageIndex >= 0 {
            mentionedImages.insert(symbol.imageIndex)
          }
        }
      }
    }

    if args.threads! {
      for (ndx, thread) in target.threads.enumerated() {
        dump(ndx: ndx, thread: thread)
      }
    } else {
      dump(ndx: target.crashingThreadNdx, thread: crashingThread)
    }

    if args.registers! == .crashedOnly {
      print("\n\nRegisters:\n")

      if let context = target.threads[target.crashingThreadNdx].context {
        showRegisters(context)
      } else {
        print(theme.info("no context for thread \(target.crashingThreadNdx)"))
      }
    }

    switch args.showImages! {
      case .none:
        break
      case .mentioned:
        let images = mentionedImages.sorted().map{ target.images[$0] }
        let omitted = target.images.count - images.count
        if omitted > 0 {
          print("\n\nImages (\(omitted) omitted):\n")
        } else {
          print("\n\nImages:\n")
        }
        print(formatter.format(images: images))
      case .all:
        print("\n\nImages:\n")
        print(formatter.format(images: target.images))
    }
  }

  static func startDebugger() {
    guard let target = target else {
      return
    }

    do {
      try target.withDebugger {

        if let ch = waitForKey("Press any key once LLDB is attached, or A to abort", timeout: nil),
           ch != UInt8(ascii: "A") && ch != UInt8(ascii: "a") {
          exit(0)
        }
      }
    } catch {
      print(theme.error("unable to spawn debugger"))
    }
  }

  static func interactWithUser() {
    guard let target = target else {
      return
    }

    while true {
      fflush(stdout)
      print(theme.prompt(">>> "), terminator: "")
      guard let input = readLine() else {
        print("")
        break
      }

      let cmd = input.split(whereSeparator: { $0.isWhitespace })

      if cmd.count < 1 {
        continue
      }

      // ###TODO: We should really replace this with something a little neater
      switch cmd[0].lowercased() {
        case "exit", "quit":
          return
        case "debug":
          startDebugger()
        case "bt", "backtrace":
          let formatter = backtraceFormatter()
          let backtrace = target.threads[currentThread].backtrace
          let formatted = formatter.format(backtrace: backtrace)

          print(formatted)
        case "thread":
          if cmd.count >= 2 {
            if let newThreadNdx = Int(cmd[1]),
               newThreadNdx >= 0 && newThreadNdx < target.threads.count {
              currentThread = newThreadNdx
            } else {
              print(theme.error("Bad thread index '\(cmd[1])'"))
              break
            }
          }

          let crashed: String
          if currentThread == target.crashingThreadNdx {
            crashed = " (crashed)"
          } else {
            crashed = ""
          }

          let thread = target.threads[currentThread]
          let backtrace = thread.backtrace
          let name = thread.name.isEmpty ? "" : " \(thread.name)"
          print("Thread \(currentThread) id=\(thread.id)\(name)\(crashed)\n")

          if let frame = backtrace.frames.drop(while: {
            $0.isSwiftRuntimeFailure
          }).first {
            let formatter = backtraceFormatter()
            let formatted = formatter.format(frame: frame)
            print("\(formatted)")
          }
          break
        case "reg", "registers":
          if let context = target.threads[currentThread].context {
            showRegisters(context)
          } else {
            print(theme.info("no context for thread \(currentThread)"))
          }
          break
        case "mem", "memory":
          if cmd.count != 2 && cmd.count != 3 {
            print("memory <start-address> [<end-address>|+<byte-count>]")
            break
          }

          guard let startAddress = parseUInt64(cmd[1]) else {
            print(theme.error("bad start address \(cmd[1])"))
            break
          }

          let count: UInt64
          if cmd.count == 3 {
            if cmd[2].hasPrefix("+") {
              guard let theCount = parseUInt64(cmd[2].dropFirst()) else {
                print(theme.error("bad byte count \(cmd[2])"))
                break
              }
              count = theCount
            } else {
              guard let addr = parseUInt64(cmd[2]) else {
                print(theme.error("bad end address \(cmd[2])"))
                break
              }
              if addr < startAddress {
                print("End address must be after start address")
                break
              }
              count = addr - startAddress
            }
          } else {
            count = 256
          }

          dumpMemory(at: startAddress, count: count)
          break
        case "process", "threads":
          print("Process \(target.pid) \"\(target.name)\" has \(target.threads.count) thread(s):\n")

          let formatter = backtraceFormatter()

          var rows: [BacktraceFormatter.TableRow] = []
          for (n, thread) in target.threads.enumerated() {
            let backtrace = thread.backtrace

            let crashed: String
            if n == target.crashingThreadNdx {
              crashed = " (crashed)"
            } else {
              crashed = ""
            }

            let selected = currentThread == n ? "▶︎" : " "
            let name = thread.name.isEmpty ? "" : " \(thread.name)"

            rows.append(.columns([ selected,
                                   "\(n)",
                                   "id=\(thread.id)\(name)\(crashed)" ]))
            if let frame = backtrace.frames.drop(while: {
              $0.isSwiftRuntimeFailure
            }).first {

              rows += formatter.formatRows(frame: frame).map{ row in
                switch row {
                  case let .columns(columns):
                    return .columns([ "", "" ] + columns)
                  default:
                    return row
                }
              }
            }
          }

          let output = BacktraceFormatter.formatTable(rows,
                                                      alignments: [
                                                        .left,
                                                        .right
                                                      ])
          print(output)
        case "images":
          let formatter = backtraceFormatter()
          let images = target.threads[currentThread].backtrace.images
          let output = formatter.format(images: images)

          print(output)
        case "set":
          if cmd.count == 1 {
            let limit: String
            if let lim = args.limit {
              limit = "\(lim)"
            } else {
              limit = "none"
            }
            let top = "\(args.top)"

            print("""
                    addresses      = \(formattingOptions.shouldShowAddresses)
                    demangle       = \(formattingOptions.shouldDemangle)
                    frame-attrs    = \(formattingOptions.shouldShowFrameAttributes)
                    image-names    = \(formattingOptions.shouldShowImageNames)
                    limit          = \(limit)
                    sanitize       = \(formattingOptions.shouldSanitizePaths)
                    source         = \(formattingOptions.shouldShowSourceCode)
                    source-context = \(formattingOptions.sourceContextLines)
                    system-frames  = \(!formattingOptions.shouldSkipSystemFrames)
                    thunks         = \(!formattingOptions.shouldSkipThunkFunctions)
                    top            = \(top)
                    """)
          } else {
            for optval in cmd[1...] {
              let parts = optval.split(separator: "=", maxSplits: 1,
                                       omittingEmptySubsequences: false)
              if parts.count == 1 {
                let option = parts[0]

                switch option {
                  case "addresses":
                    print("addresses = \(formattingOptions.shouldShowAddresses)")
                  case "demangle":
                    print("demangle = \(formattingOptions.shouldDemangle)")
                  case "frame-attrs":
                    print("frame-attrs = \(formattingOptions.shouldShowFrameAttributes)")
                  case "image-names":
                    print("image-names = \(formattingOptions.shouldShowImageNames)")
                  case "limit":
                    if let limit = args.limit {
                      print("limit = \(limit)")
                    } else {
                      print("limit = none")
                    }
                  case "sanitize":
                    print("sanitize = \(formattingOptions.shouldSanitizePaths)")
                  case "source":
                    print("source = \(formattingOptions.shouldShowSourceCode)")
                  case "source-context":
                    print("source-context = \(formattingOptions.sourceContextLines)")
                  case "system-frames":
                    print("system-frames = \(!formattingOptions.shouldSkipSystemFrames)")
                  case "thunks":
                    print("thunks = \(!formattingOptions.shouldSkipThunkFunctions)")
                  case "top":
                    print("top = \(args.top)")

                  default:
                    print(theme.error("unknown option '\(option)'"))
                }
              } else {
                let option = parts[0]
                let value = parts[1]
                var changedBacktrace = false

                switch option {
                  case "limit":
                    if value == "none" {
                      args.limit = nil
                      changedBacktrace = true
                    } else if let limit = Int(value), limit > 0 {
                      args.limit = limit
                      changedBacktrace = true
                    } else {
                      print(theme.error("bad limit value '\(value)'"))
                    }

                  case "top":
                    if let top = Int(value), top >= 0 {
                      args.top = top
                      changedBacktrace = true
                    } else {
                      print(theme.error("bad top value '\(value)'"))
                    }

                  case "source":
                    formattingOptions =
                      formattingOptions.showSourceCode(parseBool(value),
                                                       contextLines: formattingOptions.sourceContextLines)

                  case "source-context":
                    if let lines = Int(value), lines >= 0 {
                      formattingOptions =
                        formattingOptions.showSourceCode(formattingOptions.shouldShowSourceCode,
                                                         contextLines: lines)
                    } else {
                      print(theme.error("bad source-context value '\(value)'"))
                    }

                  case "thunks":
                    formattingOptions =
                      formattingOptions.skipThunkFunctions(!parseBool(value))

                  case "system-frames":
                    formattingOptions =
                      formattingOptions.skipSystemFrames(!parseBool(value))

                  case "frame-attrs":
                    formattingOptions =
                      formattingOptions.showFrameAttributes(parseBool(value))

                  case "addresses":
                    formattingOptions =
                      formattingOptions.showAddresses(parseBool(value))

                  case "sanitize":
                    formattingOptions =
                      formattingOptions.sanitizePaths(parseBool(value))

                  case "demangle":
                    formattingOptions =
                      formattingOptions.demangle(parseBool(value))

                  case "image-names":
                    formattingOptions =
                      formattingOptions.showImageNames(parseBool(value))

                  default:
                    print(theme.error("unknown option '\(option)'"))
                }

                if changedBacktrace {
                  target.redoBacktraces(limit: args.limit, top: args.top)
                }
              }
            }
          }
        case "help":
          print("""
                  Available commands:

                  backtrace  Display a backtrace.
                  bt         Synonym for backtrace.
                  debug      Attach the debugger.
                  exit       Exit interaction, allowing program to crash normally.
                  help       Display help.
                  images     List images loaded by the program.
                  mem        Synonym for memory.
                  memory     Inspect memory.
                  process    Show information about the process.
                  quit       Synonym for exit.
                  reg        Synonym for registers.
                  registers  Display the registers.
                  set        Set or show options.
                  thread     Show or set the current thread.
                  threads    Synonym for process.
                  """)
        default:
          print(theme.error("unknown command '\(cmd[0])'"))
      }

      print("")
    }
  }

  static func printableBytes(from bytes: some Sequence<UInt8>) -> String {
    // It would be nice to join these with ZWNJs to prevent ligature processing,
    // but sadly Terminal displays ZWNJ as a space character.
    return bytes.map{ byte in
      switch byte {
        case 0..<32, 127, 0x80..<0xa0:
          return theme.nonPrintable("·")
        default:
          return theme.printable(String(Unicode.Scalar(byte)))
      }
    }.joined(separator:"")
  }

  static func dumpMemory(at address: UInt64, count: UInt64) {
    guard let bytes = try? target!.reader.fetch(
            from: RemoteMemoryReader.Address(address),
            count: Int(count),
            as: UInt8.self) else {
      print("Unable to read memory")
      return
    }

    let startAddress = HostContext.stripPtrAuth(address: address)
    var ndx = 0
    while ndx < bytes.count {
      let addr = startAddress + UInt64(ndx)
      let remaining = bytes.count - ndx
      let lineChunk = 16
      let todo = min(remaining, lineChunk)
      let formattedBytes = theme.data(bytes[ndx..<ndx+todo].map{
        hex($0, withPrefix: false)
      }.joined(separator: " "))
      let printedBytes = printableBytes(from: bytes[ndx..<ndx+todo])
      let padding = String(repeating: " ",
                           count: (lineChunk - todo) * 3)

      let hexAddr = theme.address(hex(addr, withPrefix: false))
      print("\(hexAddr)  \(formattedBytes)\(padding)  \(printedBytes)")

      ndx += todo
    }
  }

  static func showRegister<T: FixedWidthInteger>(name: String, value: T) {
    let hexValue = theme.hexValue(hex(value))

    // Pad the register name
    let regPad = String(repeating: " ", count: max(3 - name.count, 0))
    let reg = theme.register(regPad + name)

    // Grab 16 bytes at each address if possible
    if let bytes = try? target!.reader.fetch(
         from: RemoteMemoryReader.Address(value),
         count: 16,
         as: UInt8.self) {
      let formattedBytes = theme.data(bytes.map{
        hex($0, withPrefix: false)
      }.joined(separator: " "))
      let printedBytes = printableBytes(from: bytes)
      print("\(reg) \(hexValue)  \(formattedBytes)  \(printedBytes)")
    } else {
      let decValue = theme.decimalValue("\(value)")
      print("\(reg) \(hexValue)  \(decValue)")
    }
  }

  static func showGPR<C: Context>(name: String, context: C, register: C.Register) {
    // Get the register contents
    let value = context.getRegister(register)!

    showRegister(name: name, value: value)
  }

  static func showGPRs<C: Context, Rs: Sequence>(_ context: C, range: Rs) where Rs.Element == C.Register {
    for reg in range {
      showGPR(name: "\(reg)", context: context, register: reg)
    }
  }

  static func x86StatusFlags<T: FixedWidthInteger>(_ flags: T) -> String {
    var status: [String] = []

    if (flags & 0x400) != 0 {
      status.append("OF")
    }
    if (flags & 0x80) != 0 {
      status.append("SF")
    }
    if (flags & 0x40) != 0 {
      status.append("ZF")
    }
    if (flags & 0x10) != 0 {
      status.append("AF")
    }
    if (flags & 0x4) != 0 {
      status.append("PF")
    }
    if (flags & 0x1) != 0 {
      status.append("CF")
    }

    return status.joined(separator: " ")
  }

  static func showRegisters(_ context: X86_64Context) {
    showGPRs(context, range: .rax ... .r15)
    showRegister(name: "rip", value: context.programCounter)

    let rflags = context.getRegister(.rflags)!
    let cs = theme.hexValue(hex(UInt16(context.getRegister(.cs)!)))
    let fs = theme.hexValue(hex(UInt16(context.getRegister(.fs)!)))
    let gs = theme.hexValue(hex(UInt16(context.getRegister(.gs)!)))

    let hexFlags = theme.hexValue(hex(rflags))
    let status = theme.flags(x86StatusFlags(rflags))

    print("")
    print("\(theme.register("rflags")) \(hexFlags)  \(status)")
    print("")
    print("\(theme.register("cs")) \(cs)  \(theme.register("fs")) \(fs)  \(theme.register("gs")) \(gs)")
  }

  static func showRegisters(_ context: I386Context) {
    showGPRs(context, range: .eax ... .edi)
    showRegister(name: "eip", value: context.programCounter)

    let eflags = UInt32(context.getRegister(.eflags)!)
    let es = theme.hexValue(hex(UInt16(context.getRegister(.es)!)))
    let cs = theme.hexValue(hex(UInt16(context.getRegister(.cs)!)))
    let ss = theme.hexValue(hex(UInt16(context.getRegister(.ss)!)))
    let ds = theme.hexValue(hex(UInt16(context.getRegister(.ds)!)))
    let fs = theme.hexValue(hex(UInt16(context.getRegister(.fs)!)))
    let gs = theme.hexValue(hex(UInt16(context.getRegister(.gs)!)))

    let hexFlags = theme.hexValue(hex(eflags))
    let status = theme.flags(x86StatusFlags(eflags))

    print("")
    print("\(theme.register("eflags")) \(hexFlags)  \(status)")
    print("")
    print("\(theme.register("es")): \(es) \(theme.register("cs")): \(cs) \(theme.register("ss")): \(ss) \(theme.register("ds")): \(ds) \(theme.register("fs")): \(fs)) \(theme.register("gs")): \(gs)")
  }

  static func showRegisters(_ context: ARM64Context) {
    showGPRs(context, range: .x0 ..< .x29)
    showGPR(name: "fp", context: context, register: .x29)
    showGPR(name: "lr", context: context, register: .x30)
    showGPR(name: "sp", context: context, register: .sp)
    showGPR(name: "pc", context: context, register: .pc)
  }

  static func showRegisters(_ context: ARMContext) {
    showGPRs(context, range: .r0 ... .r10)
    showGPR(name: "fp", context: context, register: .r11)
    showGPR(name: "ip", context: context, register: .r12)
    showGPR(name: "sp", context: context, register: .r13)
    showGPR(name: "lr", context: context, register: .r14)
    showGPR(name: "pc", context: context, register: .r15)
  }
}

#else

@main
internal struct SwiftBacktrace {
  static public func main() {
    print("swift-backtrace: not supported on this platform.")
  }
}

#endif // os(macOS)

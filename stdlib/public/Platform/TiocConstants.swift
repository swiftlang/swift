//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// Tty ioctl request constants, needed only on Darwin and FreeBSD.

// Constants available on all platforms, also available on Linux.
#if os(macOS) || os(iOS) || os(tvOS) || os(watchOS) || os(FreeBSD) || os(Haiku)

/// Set exclusive use of tty.
public var TIOCEXCL: UInt { return 0x2000740d }
/// Reset exclusive use of tty.
public var TIOCNXCL: UInt { return 0x2000740e }
/// Flush buffers.
public var TIOCFLUSH: UInt { return 0x80047410 }
/// Get line discipline.
public var TIOCGETD: UInt { return 0x4004741a }
/// Set line discipline.
public var TIOCSETD: UInt { return 0x8004741b }
/// Set break bit.
public var TIOCSBRK: UInt { return 0x2000747b }
/// Clear break bit.
public var TIOCCBRK: UInt { return 0x2000747a }
/// Set data terminal ready.
public var TIOCSDTR: UInt { return 0x20007479 }
/// Clear data terminal ready.
public var TIOCCDTR: UInt { return 0x20007478 }
/// Get pgrp of tty.
public var TIOCGPGRP: UInt { return 0x40047477 }
/// Set pgrp of tty.
public var TIOCSPGRP: UInt { return 0x80047476 }
/// Output queue size.
public var TIOCOUTQ: UInt { return 0x40047473 }
/// Simulate terminal input.
public var TIOCSTI: UInt { return 0x80017472 }
/// Void tty association.
public var TIOCNOTTY: UInt { return 0x20007471 }
/// Pty: set/clear packet mode.
public var TIOCPKT: UInt { return 0x80047470 }
/// Stop output, like `^S`.
public var TIOCSTOP: UInt { return 0x2000746f }
/// Start output, like `^Q`.
public var TIOCSTART: UInt { return 0x2000746e }
/// Set all modem bits.
public var TIOCMSET: UInt { return 0x8004746d }
/// Bis modem bits.
public var TIOCMBIS: UInt { return 0x8004746c }
/// Bic modem bits.
public var TIOCMBIC: UInt { return 0x8004746b }
/// Get all modem bits.
public var TIOCMGET: UInt { return 0x4004746a }
/// Get window size.
public var TIOCGWINSZ: UInt { return 0x40087468 }
/// Set window size.
public var TIOCSWINSZ: UInt { return 0x80087467 }
/// Pty: set/clr usr cntl mode.
public var TIOCUCNTL: UInt { return 0x80047466 }
/// Simulate `^T` status message.
public var TIOCSTAT: UInt { return 0x20007465 }
/// Become virtual console.
public var TIOCCONS: UInt { return 0x80047462 }
/// Become controlling tty.
public var TIOCSCTTY: UInt { return 0x20007461 }
/// Pty: external processing.
public var TIOCEXT: UInt { return 0x80047460 }
/// Wait till output drained.
public var TIOCDRAIN: UInt { return 0x2000745e }
/// Modem: set wait on close.
public var TIOCMSDTRWAIT: UInt { return 0x8004745b }
/// Modem: get wait on close.
public var TIOCMGDTRWAIT: UInt { return 0x4004745a }
/// Enable/get timestamp of last input event.
public var TIOCTIMESTAMP: UInt { return 0x40107459 }
/// Set ttywait timeout.
public var TIOCSDRAINWAIT: UInt { return 0x80047457 }
/// Get ttywait timeout.
public var TIOCGDRAINWAIT: UInt { return 0x40047456 }

// From ioctl_compat.h.

/// Hang up on last close.
public var TIOCHPCL: UInt { return 0x20007402 }
/// Get parameters -- gtty.
public var TIOCGETP: UInt { return 0x40067408 }
/// Set parameters -- stty.
public var TIOCSETP: UInt { return 0x80067409 }
/// As above, but no flushtty.
public var TIOCSETN: UInt { return 0x8006740a }
/// Set special characters.
public var TIOCSETC: UInt { return 0x80067411 }
/// Get special characters.
public var TIOCGETC: UInt { return 0x40067412 }
/// Bis local mode bits.
public var TIOCLBIS: UInt { return 0x8004747f }
/// Bic local mode bits.
public var TIOCLBIC: UInt { return 0x8004747e }
/// Set entire local mode word.
public var TIOCLSET: UInt { return 0x8004747d }
/// Get local modes.
public var TIOCLGET: UInt { return 0x4004747c }
/// Set local special chars.
public var TIOCSLTC: UInt { return 0x80067475 }
/// Get local special chars.
public var TIOCGLTC: UInt { return 0x40067474 }

#endif


// Darwin only constants, also available on Linux.
#if os(macOS) || os(iOS) || os(tvOS) || os(watchOS)

/// Get termios struct.
public var TIOCGETA: UInt { return 0x40487413 }
/// Set termios struct.
public var TIOCSETA: UInt { return 0x80487414 }
/// Drain output, set.
public var TIOCSETAW: UInt { return 0x80487415 }
/// Drn out, fls in, set.
public var TIOCSETAF: UInt { return 0x80487416 }
/// Pty: generate signal.
public var TIOCSIG: UInt { return 0x2000745f }
/// Get modem control state.
public var TIOCMODG: UInt { return 0x40047403 }
/// Set modem control state.
public var TIOCMODS: UInt { return 0x80047404 }
/// Internal input VSTART.
public var TIOCIXON: UInt { return 0x20007481 }
/// Internal input VSTOP.
public var TIOCIXOFF: UInt { return 0x20007480 }
/// Remote input editing.
public var TIOCREMOTE: UInt { return 0x80047469 }
/// 4.2 compatibility.
public var TIOCSCONS: UInt { return 0x20007463 }
/// Enable/get timestamp of last DCd rise.
public var TIOCDCDTIMESTAMP: UInt { return 0x40107458 }
/// Download microcode to DSI Softmodem.
public var TIOCDSIMICROCODE: UInt { return 0x20007455 }
/// Grantpt(3).
public var TIOCPTYGRANT: UInt { return 0x20007454 }
/// Ptsname(3).
public var TIOCPTYGNAME: UInt { return 0x40807453 }
/// Unlockpt(3).
public var TIOCPTYUNLK: UInt { return 0x20007452 }

#endif


// FreeBSD specific values and constants available only on FreeBSD.
#if os(FreeBSD)

/// Get termios struct.
public var TIOCGETA: UInt { return 0x402c7413 }
/// Set termios struct.
public var TIOCSETA: UInt { return 0x802c7414 }
/// Drain output, set.
public var TIOCSETAW: UInt { return 0x802c7415 }
/// Drn out, fls in, set.
public var TIOCSETAF: UInt { return 0x802c7416 }
/// Pty: generate signal.
public var TIOCSIG: UInt { return 0x2004745f }
/// Get pts number.
public var TIOCGPTN: UInt { return 0x4004740f }
/// Pts master validation.
public var TIOCPTMASTER: UInt { return 0x2000741c }
/// Get session id.
public var TIOCGSID: UInt { return 0x40047463 }

#endif

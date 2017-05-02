// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: objc_interop
// UNSUPPORTED: OS=tvos
// UNSUPPORTED: OS=watchos

import Foundation

final class Foo<T: NSCoding>: NSObject, NSCoding {
  var one, two: T

  init(one: T, two: T) {
    self.one = one
    self.two = two
  }

  @objc required convenience init(coder: NSCoder) {
    let one = coder.decodeObject(forKey: "one") as! T
    let two = coder.decodeObject(forKey :"two") as! T
    self.init(one: one, two: two)
  }

  @objc(encodeWithCoder:) func encode(with encoder: NSCoder) {
    encoder.encode(one, forKey: "one")
    encoder.encode(two, forKey: "two")
  }
}

// FIXME: W* macro equivalents should be in the Darwin/Glibc overlay
func WIFEXITED(_ status: Int32) -> Bool {
  return (status & 0o177) == 0
}
func WEXITSTATUS(_ status: Int32) -> Int32 {
  return (status >> 8) & 0xFF
}

// FIXME: "environ" should be in the Darwin overlay too
@_silgen_name("_NSGetEnviron")
func _NSGetEnviron() -> UnsafeMutablePointer<UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>>

var environ: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?> {
  return _NSGetEnviron().pointee
}

func driver() {
  // Create a pipe to connect the archiver to the unarchiver.
  var pipes: [Int32] = [0, 0]
  guard pipe(&pipes) == 0 else { fatalError("pipe failed") }

  let pipeRead = pipes[0], pipeWrite = pipes[1]

  var archiver: pid_t = 0, unarchiver: pid_t = 0

  let envp = environ

  do {
    // Set up the archiver's stdout to feed into our pipe.
    var archiverActions: posix_spawn_file_actions_t?
    guard posix_spawn_file_actions_init(&archiverActions) == 0 else {
      fatalError("posix_spawn_file_actions_init failed")
    }
    defer { posix_spawn_file_actions_destroy(&archiverActions) }
    guard posix_spawn_file_actions_adddup2(&archiverActions,
                                           pipeWrite,
                                           STDOUT_FILENO) == 0
       && posix_spawn_file_actions_addclose(&archiverActions,
                                            pipeRead) == 0
       else {
      fatalError("posix_spawn_file_actions_add failed")
    }

    // Spawn the archiver process.
    let str: StaticString = "-archive"
    let optStr = UnsafeMutableRawPointer(mutating: str.utf8Start).bindMemory(
      to: CChar.self, capacity: str.utf8CodeUnitCount)
    let archiverArgv: [UnsafeMutablePointer<Int8>?] = [
      CommandLine.unsafeArgv[0], optStr, nil
    ]
    guard posix_spawn(&archiver, CommandLine.unsafeArgv[0],
                      &archiverActions, nil,
                      archiverArgv, envp) == 0 else {
      fatalError("posix_spawn failed")
    }
  }

  do {
    // Set up the unarchiver's stdin to read from our pipe.
    var unarchiverActions: posix_spawn_file_actions_t?
    guard posix_spawn_file_actions_init(&unarchiverActions) == 0 else {
      fatalError("posix_spawn_file_actions_init failed")
    }
    defer { posix_spawn_file_actions_destroy(&unarchiverActions) }
    guard posix_spawn_file_actions_adddup2(&unarchiverActions,
                                           pipeRead,
                                           STDIN_FILENO) == 0
       && posix_spawn_file_actions_addclose(&unarchiverActions,
                                            pipeWrite) == 0
       else {
      fatalError("posix_spawn_file_actions_add failed")
    }

    // Spawn the unarchiver process.
    let str = "-unarchive" as StaticString
    let optStr = UnsafeMutableRawPointer(mutating: str.utf8Start).bindMemory(
      to: CChar.self, capacity: str.utf8CodeUnitCount)
    var unarchiver: pid_t = 0
    let unarchiverArgv: [UnsafeMutablePointer<Int8>?] = [
      CommandLine.unsafeArgv[0], optStr, nil
    ]
    guard posix_spawn(&unarchiver, CommandLine.unsafeArgv[0],
                      &unarchiverActions, nil,
                      unarchiverArgv, envp) == 0 else {
      fatalError("posix_spawn failed")
    }
  }

  // Wash our hands of the pipe, now that the subprocesses have started.
  close(pipeRead)
  close(pipeWrite)

  // Wait for the subprocesses to finish.
  var waiting: Set<pid_t> = [archiver, unarchiver]
  while !waiting.isEmpty {
    var status: Int32 = 0
    let pid = wait(&status)
    if pid == -1 {
      // If the error was EINTR, just wait again.
      if errno == EINTR { continue }
      // If we have no children to wait for, stop.
      if errno == ECHILD { break }
      fatalError("wait failed")
    }
    waiting.remove(pid)
    // Ensure the process exited successfully.
    guard WIFEXITED(status) && WEXITSTATUS(status) == 0 else {
      fatalError("subprocess exited abnormally")
    }
  }
}

func archive() {
  let data = NSMutableData()
  let archiver = NSKeyedArchiver(forWritingWith: data)
  archiver.encode(Foo<NSString>(one: "one", two: "two"), forKey: "strings")
  archiver.encode(Foo<NSNumber>(one: 1, two: 2), forKey: "numbers")
  archiver.finishEncoding()

  // Output the archived data over stdout, which should be piped to stdin
  // on the unarchiver process.
  while true {
    let status = write(STDOUT_FILENO, data.bytes, data.length)
    if status == data.length { break }
    if errno == EINTR { continue }
    fatalError("write failed")
  }
}

func unarchive() {
  // FIXME: Pre-instantiate the generic classes that were archived, since
  // the ObjC runtime doesn't know how.
  NSStringFromClass(Foo<NSNumber>.self)
  NSStringFromClass(Foo<NSString>.self)

  // Read in the data from stdin, where the archiver process should have
  // written it.
  var rawData: [UInt8] = []

  var buffer = [UInt8](repeating: 0, count: 4096)

  while true {
    let count = read(STDIN_FILENO, &buffer, 4096)
    if count == 0 { break }
    if count == -1 {
      if errno == EINTR { continue }
      fatalError("read failed")
    }
    rawData += buffer[0..<count]
  }

  // Feed it into an unarchiver.
  let data = NSData(bytes: rawData, length: rawData.count)
  let unarchiver = NSKeyedUnarchiver(forReadingWith: data as Data)

  guard let strings
      = unarchiver.decodeObject(forKey: "strings") as? Foo<NSString> else {
    fatalError("unable to unarchive Foo<NSString>")
  }
  guard let numbers
      = unarchiver.decodeObject(forKey: "numbers") as? Foo<NSNumber> else {
    fatalError("unable to unarchive Foo<NSNumber>")
  }

  // CHECK-LABEL: <_T04main3FooCySo8NSStringCGD: {{0x[0-9a-f]+}}> #0
  // CHECK:         one: one
  // CHECK:         two: two
  // CHECK-LABEL: <_T04main3FooCySo8NSNumberCGD: {{0x[0-9a-f]+}}> #0
  // CHECK:         one: 1
  // CHECK:         two: 2
  dump(strings)
  dump(numbers)
}

// Pick a mode based on the command-line arguments.
// The test launches as a "driver" which then respawns itself into reader
// and writer subprocesses.
if CommandLine.arguments.count < 2 {
  driver()
} else if CommandLine.arguments[1] == "-archive" {
  archive()
} else if CommandLine.arguments[1] == "-unarchive" {
  unarchive()
} else {
  fatalError("invalid commandline argument")
}


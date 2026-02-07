// RUN: %empty-directory(%t)
// RUN: %target-build-swift %S/Inputs/CrashSubprocess.swift -parse-as-library -Onone -g -o %t/crash
// RUN: %target-codesign %t/crash

// RUN: %target-build-swift %s -o %t/crash-host

// RUN: env SWIFT_BACKTRACE=enable=yes,cache=no,interactive=no %target-run %t/crash-host %t/crash | %FileCheck %s

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: asan

// REQUIRES: executable_test
// REQUIRES: backtracing
// REQUIRES: OS=linux-gnu || OS=macosx

// COM: this test would need significant rework on Windows/WinSDK, but is not critical function so probably not worth porting the test to that platform

#if os(macOS)
internal import Darwin
#elseif canImport(Glibc)
internal import Glibc
#elseif canImport(Musl)
internal import Musl
#endif

guard CommandLine.arguments.count > 1 else {
    fputs("Usage: \(CommandLine.arguments[0]) <path-to-crash>\n", stderr)
    exit(1)
}

let crashPath = CommandLine.arguments[1]

// CHECK: host stdout is a tty: 0
print("host stdout is a tty: \(isatty(1))")
// (piped output on linux is fully buffered by default)
fflush(stdout)

// CHECK: host stderr is a tty: 0
print("host stderr is a tty: \(isatty(2))")
fflush(stdout)

var masterFD: Int32 = 0
var slaveFD: Int32 = 0

let slaveName = UnsafeMutablePointer<CChar>.allocate(capacity: 1024)

guard openpty(&masterFD, &slaveFD, slaveName, nil, nil) == 0 else {
  perror("openpty")
  exit(1)
}

// CHECK: slave is: /dev/{{.+}}
print("slave is: \(String(cString: slaveName))")
slaveName.deallocate()
fflush(stdout)

// try to avoid using Foundation, use Posix primitives instead
var childPid: pid_t = 0

#if os(macOS)
var childFileActions: posix_spawn_file_actions_t?
posix_spawn_file_actions_init(&childFileActions)
posix_spawn_file_actions_adddup2(&childFileActions, slaveFD, STDOUT_FILENO)
posix_spawn_file_actions_adddup2(&childFileActions, slaveFD, STDERR_FILENO)

guard posix_spawn(&childPid, Array<CChar>(crashPath.utf8CString), &childFileActions, nil, nil, environ) == 0 else {
  perror("unable to spawn child")
  exit(1)
}

posix_spawn_file_actions_destroy(&childFileActions)

#elseif os(Linux)
var childFileActions = posix_spawn_file_actions_t()
posix_spawn_file_actions_init(&childFileActions)
posix_spawn_file_actions_adddup2(&childFileActions, slaveFD, STDOUT_FILENO)
posix_spawn_file_actions_adddup2(&childFileActions, slaveFD, STDERR_FILENO)

var argv: [UnsafeMutablePointer<CChar>?] = [nil]

guard posix_spawn(&childPid, Array<CChar>(crashPath.utf8CString), &childFileActions, nil, argv, environ) == 0 else {
  perror("unable to spawn child")
  exit(1)
}

posix_spawn_file_actions_destroy(&childFileActions)

#endif

// CHECK: child pid is {{[0-9]+}}
print("child pid is \(childPid)")
fflush(stdout)

// the parent process should now close this FD as it is used by the child for stdout/stderr
close(slaveFD)

// CHECK: subprocess stdout is a tty: 1
// CHECK: subprocess stderr is a tty: 1

// CHECK: About to crash

// CHECK: 💣{{.*}}Program crashed: Bad pointer dereference at 0x{{0+}}4
// CHECK: Thread 0 {{(".*" )?}}crashed:
// CHECK: Backtrace took {{[0-9.]+}}s

// allow the child process time to start, and crash...
sleep(1)

// now read the standard output and error from our master pty
// and write it to our own standard output for scanning by FileCheck
let bufferSize = 4096
let buffer = UnsafeMutablePointer<UInt8>.allocate(capacity: bufferSize)
var bytesRead = read(masterFD, buffer, bufferSize)

while bytesRead > 0 {
  write(1, buffer, bytesRead)
  bytesRead = read(masterFD, buffer, bufferSize)
}

close(masterFD)
buffer.deallocate()

// CHECK-NOT: child exited with status: 0
var childExitStatus: Int32 = 0
waitpid(childPid, &childExitStatus, 0)
print("child exited with status: \(childExitStatus)")

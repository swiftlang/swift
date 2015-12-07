// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
#if os(Linux)
  import Glibc
#else
  import Darwin
#endif

var POSIXTests = TestSuite("POSIXTests")

#if !os(Linux)
  let semaphoreName = "TestSem"
#endif
let fn = "test.txt"

POSIXTests.setUp {
  #if !os(Linux)
    sem_unlink(semaphoreName)
  #endif
  unlink(strdup(fn))
}

// Failed semaphore creation.
POSIXTests.test("sem_open fail") {
  let sem = sem_open(semaphoreName, 0)
  expectEqual(SEM_FAILED, sem)
  expectEqual(ENOENT, errno)
}

// Successful semaphore creation.
POSIXTests.test("sem_open success") {
  let sem = sem_open(semaphoreName, O_CREAT, 0o777, 1)
  expectNotEqual(SEM_FAILED, sem)

  let res = sem_close(sem)
  expectEqual(0, res)

  let res2 = sem_unlink(semaphoreName)
  expectEqual(0, res2)
}

// Successful semaphore creation with O_EXCL.
POSIXTests.test("sem_open O_EXCL success") {
  let sem = sem_open(semaphoreName, O_CREAT | O_EXCL, 0o777, 1)
  expectNotEqual(SEM_FAILED, sem)

  let res = sem_close(sem)
  expectEqual(0, res)

  let res2 = sem_unlink(semaphoreName)
  expectEqual(0, res2)
}

// Successful creation and re-obtaining of existing semaphore.
POSIXTests.test("sem_open existing") {
  let sem = sem_open(semaphoreName, O_CREAT, 0o777, 1)
  expectNotEqual(SEM_FAILED, sem)

  let sem2 = sem_open(semaphoreName, 0)
  // Here, we'd like to test that the semaphores are the same, but it's quite
  // difficult.
  expectNotEqual(SEM_FAILED, sem2)

  let res = sem_close(sem)
  expectEqual(0, res)

  let res2 = sem_unlink(semaphoreName)
  expectEqual(0, res2)
}

// Fail because the semaphore already exists.
POSIXTests.test("sem_open existing O_EXCL fail") {
  let sem = sem_open(semaphoreName, O_CREAT, 0o777, 1)
  expectNotEqual(SEM_FAILED, sem)

  let sem2 = sem_open(semaphoreName, O_CREAT | O_EXCL, 0o777, 1)
  expectEqual(SEM_FAILED, sem2)
  expectEqual(EEXIST, errno)

  let res = sem_close(sem)
  expectEqual(0, res)

  let res2 = sem_unlink(semaphoreName)
  expectEqual(0, res2)
}
	
// Fail because file doesn't exist.
POSIXTests.test("fcntl fail") {
  let fd = open(strdup(fn), 0)
  expectEqual(-1, fd)
  expectEqual(ENOENT, errno)
	
  let _ = fcntl(fd, cmd: F_GETFL)
  expectEqual(EBADF, errno)
}

// Change modes on existing file.
POSIXTests.test("fcntl F_GETFL/F_SETFL success with file") {
  // Create and open file.
  system("touch \(fn)")
  let fd = open(strdup(fn), 0)
  expectGT(0, fd)
	
  var flags = fcntl(fd, cmd: F_GETFL)
  expectGE(0, flags)
	
  // Change to APPEND mode...
  var rc = fcntl(fd, cmd: F_SETFL, value: O_APPEND)
  expectEqual(0, rc)
	
  flags = fcntl(fd, cmd: F_GETFL)
  expectEqual(flags | O_APPEND, flags)
	
  // Change back...
  rc = fcntl(fd, cmd: F_SETFL, value: 0)
  expectEqual(0, rc)

  flags = fcntl(fd, cmd: F_GETFL)
  expectGE(0, flags)
	
  // Clean up...
  rc = close(fd)
  expectEqual(0, rc)
	
  rc = unlink(strdup(fn))
  expectEqual(0, rc)
}

POSIXTests.test("fcntl block and unblocking sockets success") {
  // Create socket, note: socket created by default in blocking mode...
  let sock = socket(PF_INET, 1, 0)
  expectGT(0, sock)
	
  var flags = fcntl(sock, cmd:F_GETFL)
  expectGE(0, flags)
	
  // Change mode of socket to non-blocking...
  var rc = fcntl(sock, cmd: F_SETFL, value: flags | O_NONBLOCK)
  expectEqual(0, rc)
	
  flags = fcntl(sock, cmd: F_GETFL)
  expectGE(flags | O_NONBLOCK, flags)
	
  // Change back to blocking...
  rc = fcntl(sock, cmd: F_SETFL, value: flags & ~O_NONBLOCK)
  expectEqual(0, rc)
	
  flags = fcntl(sock, cmd: F_GETFL)
  expectGE(0, flags)
	
  // Clean up...
  rc = close(sock)
  expectEqual(0, rc)
}

POSIXTests.test("fcntl locking and unlocking success") {
  // Create the file and add data to it...
  var fd = open(strdup(fn), O_CREAT | O_WRONLY, 0o666)
  expectGT(0, fd)
	
  let data = "Testing 1 2 3"
  let bytesWritten = write(fd, data, data.characters.count)
  expectEqual(data.characters.count, bytesWritten)
	
  var rc = close(fd)
  expectEqual(0, rc)
	
  // Re-open the file...
  fd = open(strdup(fn), 0)
  expectGT(0, fd)
	
  // Lock for reading...
  var flck = flock()
  flck.l_type = Int16(F_RDLCK)
  flck.l_len = off_t(data.characters.count)
  rc = fcntl(fd, cmd: F_SETLK, ptr: &flck)
  expectEqual(0, rc)
	
  // Unlock for reading...
  flck = flock()
  flck.l_type = Int16(F_UNLCK)
  rc = fcntl(fd, cmd: F_SETLK, ptr: &flck)
  expectEqual(0, rc)
	
  // Clean up...
  rc = close(fd)
  expectEqual(0, rc)
	
  rc = unlink(strdup(fn))
  expectEqual(0, rc)
}

runAllTests()


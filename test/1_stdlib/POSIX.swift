// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// XFAIL: linux
// FIXME: Glibc needs the same overlay.

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

// Semaphore Operation have not been moved over to Linux yet. See FIXME above.
#if !os(Linux)
	
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
	
#endif

// Fail because file doesn't exist.
POSIXTests.test("fcntl fail") {
  let fd = open(strdup(fn), 0)
  expectEqual(-1, fd)
  expectEqual(ENOENT, errno)
	
  let flags = fcntl(fd, cmd: F_GETFL)
  expectEqual(-1, flags)
  expectEqual(ENOENT, errno)
}

// Change modes on existing file.
POSIXTests.test("fcntl F_GETFL/F_SETFL success with file") {
  // Create and open file.
  system("touch \(fn)")
  let fd = open(strdup(fn), 0)
  expectGT(0, fd)
	
  var flags = fcntl(fd, cmd: F_GETFL)
  expectEqual(0, flags)
	
  // Change to APPEND mode...
  var rc = fcntl(fd, cmd: F_SETFL, O_APPEND)
  expectEqual(0, rc)
	
  flags = fcntl(fd, cmd: F_GETFL)
  expectEqual(O_APPEND, flags)
	
  // Change back...
  rc = fcntl(fd, cmd: F_SETFL, 0)
  expectEqual(0, rc)

  flags = fcntl(fd, cmd: F_GETFL)
  expectEqual(0, flags)
	
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
	
  var flags = fcntl(sock, F_GETFL)
  expectEqual(2, flags)
	
  // Change mode of socket to non-blocking...
  var rc = fcntl(sock, cmd: F_SETFL, value: flags | ~O_NONBLOCK)
  expectEqual(0, rc)
	
  flags = fcntl(sock, F_GETFL)
  expectEqual(4194510, flags)
	
  // Change back to blocking...
  rc = fcntl(sock, cmd: F_SETFL, value: flags & ~O_NONBLOCK)
  expectEqual(0, rc)
	
  flags = fcntl(sock, F_GETFL)
  expectEqual(2, flags)
	
  // Clean up...
  rc = close(sock)
  expectEqual(0, rc)
}

POSIXTests.test("fcntl locking and unlocking success") {
  // Create the file and add data to it...
  let stream = fopen(strdup(fn), "r+")
  expectGT(0, stream)
	
  let data = "Testing 1 2 3"
  var bytesWritten = fwrite(data, 1, data.characters.count, stream)
  expectEqual(data.characters.count, bytesWritten)
	
  var rc = fclose(stream)
  expectEqual(0, rc)
	
  // Re-open the file...
  var fd = open(strdup(fn), 0)
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


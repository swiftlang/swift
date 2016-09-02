// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// Android Bionic does not provide a working implementation of
// <semaphore.h>.
// XFAIL: OS=linux-androideabi

import StdlibUnittest
#if os(Linux)
  import Glibc
#else
  import Darwin
#endif


var POSIXTests = TestSuite("POSIXTests")

let semaphoreName = "TestSem"
let fn = "test.txt"

POSIXTests.setUp {
  sem_unlink(semaphoreName)
  unlink(fn)
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

// Fail because the file descriptor is invalid.
POSIXTests.test("ioctl(CInt, UInt, CInt): fail") {
  let fd = open(fn, 0)
  expectEqual(-1, fd)
  expectEqual(ENOENT, errno)
	
  // A simple check to verify that ioctl is available
  let _ = ioctl(fd, 0, 0)
  expectEqual(EBADF, errno)
}   

#if os(Linux)
// Successful creation of a socket and listing interfaces
POSIXTests.test("ioctl(CInt, UInt, UnsafeMutableRawPointer): listing interfaces success") {
  // Create a socket
  let sock = socket(PF_INET, 1, 0)
  expectGT(Int(sock), 0)

  // List interfaces
  var ic = ifconf()
  let io = ioctl(sock, UInt(SIOCGIFCONF), &ic);
  expectGE(io, 0)

  //Cleanup
  let res = close(sock)
  expectEqual(0, res)
}
#endif

// Fail because file doesn't exist.
POSIXTests.test("fcntl(CInt, CInt): fail") {
  let fd = open(fn, 0)
  expectEqual(-1, fd)
  expectEqual(ENOENT, errno)
	
  let _ = fcntl(fd, F_GETFL)
  expectEqual(EBADF, errno)
}

// Change modes on existing file.
POSIXTests.test("fcntl(CInt, CInt): F_GETFL/F_SETFL success with file") {
  // Create and open file.
  let fd = open(fn, O_CREAT, 0o666)
  expectGT(Int(fd), 0)
	
  var flags = fcntl(fd, F_GETFL)
  expectGE(Int(flags), 0)
	
  // Change to APPEND mode...
  var rc = fcntl(fd, F_SETFL, O_APPEND)
  expectEqual(0, rc)
	
  flags = fcntl(fd, F_GETFL)
  expectEqual(flags | O_APPEND, flags)
	
  // Change back...
  rc = fcntl(fd, F_SETFL, 0)
  expectEqual(0, rc)

  flags = fcntl(fd, F_GETFL)
  expectGE(Int(flags), 0)
	
  // Clean up...
  rc = close(fd)
  expectEqual(0, rc)
	
  rc = unlink(fn)
  expectEqual(0, rc)
}

POSIXTests.test("fcntl(CInt, CInt, CInt): block and unblocking sockets success") {
  // Create socket, note: socket created by default in blocking mode...
  let sock = socket(PF_INET, 1, 0)
  expectGT(Int(sock), 0)
	
  var flags = fcntl(sock, F_GETFL)
  expectGE(Int(flags), 0)
	
  // Change mode of socket to non-blocking...
  var rc = fcntl(sock, F_SETFL, flags | O_NONBLOCK)
  expectEqual(0, rc)
	
  flags = fcntl(sock, F_GETFL)
  expectEqual((flags | O_NONBLOCK), flags)
	
  // Change back to blocking...
  rc = fcntl(sock, F_SETFL, flags & ~O_NONBLOCK)
  expectEqual(0, rc)
	
  flags = fcntl(sock, F_GETFL)
  expectGE(Int(flags), 0)
	
  // Clean up...
  rc = close(sock)
  expectEqual(0, rc)
}

POSIXTests.test("fcntl(CInt, CInt, UnsafeMutableRawPointer): locking and unlocking success") {
  // Create the file and add data to it...
  var fd = open(fn, O_CREAT | O_WRONLY, 0o666)
  expectGT(Int(fd), 0)
	
  let data = "Testing 1 2 3"
  let bytesWritten = write(fd, data, data.utf8.count)
  expectEqual(data.utf8.count, bytesWritten)
	
  var rc = close(fd)
  expectEqual(0, rc)
	
  // Re-open the file...
  fd = open(fn, 0)
  expectGT(Int(fd), 0)
	
  // Lock for reading...
  var flck = flock()
  flck.l_type = Int16(F_RDLCK)
  flck.l_len = off_t(data.utf8.count)
  rc = fcntl(fd, F_SETLK, &flck)
  expectEqual(0, rc)
	
  // Unlock for reading...
  flck = flock()
  flck.l_type = Int16(F_UNLCK)
  rc = fcntl(fd, F_SETLK, &flck)
  expectEqual(0, rc)
	
  // Clean up...
  rc = close(fd)
  expectEqual(0, rc)
	
  rc = unlink(fn)
  expectEqual(0, rc)
}

runAllTests()


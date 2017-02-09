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

#if os(OSX) || os(iOS) || os(tvOS) || os(watchOS)
/// Enumeration describing Mach error codes.
@objc public enum MachErrorCode : Int32 {
  case success                  = 0

  /// Specified address is not currently valid.
  case invalidAddress           = 1

  /// Specified memory is valid, but does not permit the required
  /// forms of access.
  case protectionFailure        = 2

  /// The address range specified is already in use, or no address
  /// range of the size specified could be found.  
  case noSpace                  = 3

  /// The function requested was not applicable to this type of
  /// argument, or an argument is invalid.
  case invalidArgument          = 4

  /// The function could not be performed.  A catch-all.
  case failure                  = 5

  /// A system resource could not be allocated to fulfill this
  /// request.  This failure may not be permanent.
  case resourceShortage         = 6

  /// The task in question does not hold receive rights for the port
  /// argument.
  case notReceiver              = 7

  /// Bogus access restriction.
  case noAccess                 = 8

  /// During a page fault, the target address refers to a memory
  /// object that has been destroyed.  This failure is permanent.
  case memoryFailure            = 9

  /// During a page fault, the memory object indicated that the data
  /// could not be returned.  This failure may be temporary; future
  /// attempts to access this same data may succeed, as defined by the
  /// memory object.
  case memoryError              = 10

  /// The receive right is already a member of the portset.
  case alreadyInSet             = 11

  /// The receive right is not a member of a port set.
  case notInSet                 = 12

  /// The name already denotes a right in the task.
  case nameExists               = 13

  /// The operation was aborted.  Ipc code will catch this and reflect
  /// it as a message error.
  case aborted                  = 14

  /// The name doesn't denote a right in the task.
  case invalidName              = 15

  /// Target task isn't an active task.
  case invalidTask              = 16

  /// The name denotes a right, but not an appropriate right.
  case invalidRight             = 17

  /// A blatant range error.
  case invalidValue             = 18

  /// Operation would overflow limit on user-references.
  case userReferencesOverflow   = 19

  /// The supplied (port) capability is improper.
  case invalidCapability        = 20

  /// The task already has send or receive rights for the port under
  /// another name.
  case rightExists              = 21

  /// Target host isn't actually a host.
  case invalidHost              = 22

  /// An attempt was made to supply "precious" data for memory that is
  /// already present in a memory object.
  case memoryPresent            = 23

  /// A page was requested of a memory manager via
  /// memory_object_data_request for an object using a
  /// MEMORY_OBJECT_COPY_CALL strategy, with the VM_PROT_WANTS_COPY
  /// flag being used to specify that the page desired is for a copy
  /// of the object, and the memory manager has detected the page was
  /// pushed into a copy of the object while the kernel was walking
  /// the shadow chain from the copy to the object. This error code is
  /// delivered via memory_object_data_error and is handled by the
  /// kernel (it forces the kernel to restart the fault). It will not
  /// be seen by users.
  case memoryDataMoved          = 24

  /// A strategic copy was attempted of an object upon which a quicker
  /// copy is now possible.  The caller should retry the copy using
  /// vm_object_copy_quickly. This error code is seen only by the
  /// kernel.
  case memoryRestartCopy        = 25

  /// An argument applied to assert processor set privilege was not a
  /// processor set control port.
  case invalidProcessorSet      = 26

  /// The specified scheduling attributes exceed the thread's limits.
  case policyLimit              = 27

  /// The specified scheduling policy is not currently enabled for the
  /// processor set.
  case invalidPolicy            = 28

  /// The external memory manager failed to initialize the memory object.
  case invalidObject            = 29

  /// A thread is attempting to wait for an event for which there is
  /// already a waiting thread.
  case alreadyWaiting           = 30

  /// An attempt was made to destroy the default processor set.
  case defaultSet               = 31

  /// An attempt was made to fetch an exception port that is
  /// protected, or to abort a thread while processing a protected
  /// exception.
  case exceptionProtected       = 32

  /// A ledger was required but not supplied.
  case invalidLedger            = 33

  /// The port was not a memory cache control port.
  case invalidMemoryControl     = 34

  /// An argument supplied to assert security privilege was not a host
  /// security port.
  case invalidSecurity          = 35

  /// thread_depress_abort was called on a thread which was not
  /// currently depressed.
  case notDepressed             = 36

  /// Object has been terminated and is no longer available.
  case terminated               = 37

  /// Lock set has been destroyed and is no longer available.
  case lockSetDestroyed         = 38

  /// The thread holding the lock terminated before releasing the lock.
  case lockUnstable             = 39

  /// The lock is already owned by another thread.
  case lockOwned                = 40

  /// The lock is already owned by the calling thread.
  case lockOwnedSelf            = 41

  /// Semaphore has been destroyed and is no longer available.
  case semaphoreDestroyed       = 42

  /// Return from RPC indicating the target server was terminated
  /// before it successfully replied.
  case rpcServerTerminated      = 43

  /// Terminate an orphaned activation.
  case rpcTerminateOrphan       = 44

  /// Allow an orphaned activation to continue executing.
  case rpcContinueOrphan        = 45

  /// Empty thread activation (No thread linked to it).
  case notSupported             = 46

  /// Remote node down or inaccessible.
  case nodeDown                 = 47

  /// A signalled thread was not actually waiting.
  case notWaiting               = 48

  /// Some thread-oriented operation (semaphore_wait) timed out.
  case operationTimedOut        = 49

  /// During a page fault, indicates that the page was rejected as a
  /// result of a signature check.
  case codesignError            = 50

  /// The requested property cannot be changed at this time.
  case policyStatic             = 51
}
#endif // os(OSX) || os(iOS) || os(tvOS) || os(watchOS)

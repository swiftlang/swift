#if os(OSX) || os(iOS) || os(tvOS) || os(watchOS)
/// Enumeration describing Mach error codes.
@objc public enum _MachError : CInt {
  case KERN_SUCCESS                   = 0

  /// Specified address is not currently valid.
  case KERN_INVALID_ADDRESS           = 1

  /// Specified memory is valid, but does not permit the required
  /// forms of access.
  case KERN_PROTECTION_FAILURE        = 2

  /// The address range specified is already in use, or no address
  /// range of the size specified could be found.  
  case KERN_NO_SPACE                  = 3

  /// The function requested was not applicable to this type of
  /// argument, or an argument is invalid.
  case KERN_INVALID_ARGUMENT          = 4

  /// The function could not be performed.  A catch-all.
  case KERN_FAILURE                   = 5

  /// A system resource could not be allocated to fulfill this
  /// request.  This failure may not be permanent.
  case KERN_RESOURCE_SHORTAGE         = 6

  /// The task in question does not hold receive rights for the port
  /// argument.
  case KERN_NOT_RECEIVER              = 7

  /// Bogus access restriction.
  case KERN_NO_ACCESS                 = 8

  /// During a page fault, the target address refers to a memory
  /// object that has been destroyed.  This failure is permanent.
  case KERN_MEMORY_FAILURE            = 9

  /// During a page fault, the memory object indicated that the data
  /// could not be returned.  This failure may be temporary; future
  /// attempts to access this same data may succeed, as defined by the
  /// memory object.
  case KERN_MEMORY_ERROR              = 10

  /// The receive right is already a member of the portset.
  case KERN_ALREADY_IN_SET            = 11

  /// The receive right is not a member of a port set.
  case KERN_NOT_IN_SET                = 12

  /// The name already denotes a right in the task.
  case KERN_NAME_EXISTS               = 13

  /// The operation was aborted.  Ipc code will catch this and reflect
  /// it as a message error.
  case KERN_ABORTED                   = 14

  /// The name doesn't denote a right in the task.
  case KERN_INVALID_NAME              = 15

  /// Target task isn't an active task.
  case KERN_INVALID_TASK              = 16

  /// The name denotes a right, but not an appropriate right.
  case KERN_INVALID_RIGHT             = 17

  /// A blatant range error.
  case KERN_INVALID_VALUE             = 18

  /// Operation would overflow limit on user-references.
  case KERN_UREFS_OVERFLOW            = 19

  /// The supplied (port) capability is improper.
  case KERN_INVALID_CAPABILITY        = 20

  /// The task already has send or receive rights for the port under
  /// another name.
  case KERN_RIGHT_EXISTS              = 21

  /// Target host isn't actually a host.
  case KERN_INVALID_HOST              = 22

  /// An attempt was made to supply "precious" data for memory that is
  /// already present in a memory object.
  case KERN_MEMORY_PRESENT            = 23

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
  case KERN_MEMORY_DATA_MOVED         = 24
                

  /// A strategic copy was attempted of an object upon which a quicker
  /// copy is now possible.  The caller should retry the copy using
  /// vm_object_copy_quickly. This error code is seen only by the
  /// kernel.
  case KERN_MEMORY_RESTART_COPY       = 25

  /// An argument applied to assert processor set privilege was not a
  /// processor set control port.
  case KERN_INVALID_PROCESSOR_SET     = 26

  /// The specified scheduling attributes exceed the thread's limits.
  case KERN_POLICY_LIMIT              = 27

  /// The specified scheduling policy is not currently enabled for the
  /// processor set.
  case KERN_INVALID_POLICY            = 28

  /// The external memory manager failed to initialize the memory object.
  case KERN_INVALID_OBJECT            = 29

  /// A thread is attempting to wait for an event for which there is
  /// already a waiting thread.
  case KERN_ALREADY_WAITING           = 30

  /// An attempt was made to destroy the default processor set.
  case KERN_DEFAULT_SET               = 31

  /// An attempt was made to fetch an exception port that is
  /// protected, or to abort a thread while processing a protected
  /// exception.
  case KERN_EXCEPTION_PROTECTED       = 32

  /// A ledger was required but not supplied.
  case KERN_INVALID_LEDGER            = 33

  /// The port was not a memory cache control port.
  case KERN_INVALID_MEMORY_CONTROL    = 34

  /// An argument supplied to assert security privilege was not a host
  /// security port.
  case KERN_INVALID_SECURITY          = 35

  /// thread_depress_abort was called on a thread which was not
  /// currently depressed.
  case KERN_NOT_DEPRESSED             = 36
                
  /// Object has been terminated and is no longer available
  case KERN_TERMINATED                = 37

  /// Lock set has been destroyed and is no longer available.
  case KERN_LOCK_SET_DESTROYED        = 38

  /// The thread holding the lock terminated before releasing the lock
  case KERN_LOCK_UNSTABLE             = 39

  /// The lock is already owned by another thread
  case KERN_LOCK_OWNED                = 40

  /// The lock is already owned by the calling thread
  case KERN_LOCK_OWNED_SELF           = 41

  /// Semaphore has been destroyed and is no longer available.
  case KERN_SEMAPHORE_DESTROYED       = 42

  /// Return from RPC indicating the target server was terminated
  /// before it successfully replied
  case KERN_RPC_SERVER_TERMINATED     = 43

  /// Terminate an orphaned activation.
  case KERN_RPC_TERMINATE_ORPHAN      = 44

  /// Allow an orphaned activation to continue executing.
  case KERN_RPC_CONTINUE_ORPHAN       = 45

  /// Empty thread activation (No thread linked to it)
  case KERN_NOT_SUPPORTED             = 46

  /// Remote node down or inaccessible.
  case KERN_NODE_DOWN                 = 47

  /// A signalled thread was not actually waiting.
  case KERN_NOT_WAITING               = 48

  /// Some thread-oriented operation (semaphore_wait) timed out
  case KERN_OPERATION_TIMED_OUT       = 49

  /// During a page fault, indicates that the page was rejected as a
  /// result of a signature check.
  case KERN_CODESIGN_ERROR            = 50

  /// The requested property cannot be changed at this time.
  case KERN_POLICY_STATIC             = 51
}
#endif

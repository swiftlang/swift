:orphan:

.. @raise litre.TestsAreMissing

==================
C interoperability
==================

This document is a collection of random advice about C interoperability.

`bind()` function and subtyping between `sockaddr` and `sockaddr_in`
====================================================================

The `bind()` function is declared as accepting a parameter of type
`UnsafePointer<sockaddr>`, but you are required to pass a pointer to an
instance of a subtype, for example, `sockaddr_in`.  The naive code is wrong and
memory-unsafe::

  var sa = sockaddr_in()
  bind(..., UnsafePointer<sockaddr>(&sa), ...)

Let's look at a simpler example.  This code::

  func foo(x: UnsafePointer<SomeType>) { ... }

  foo(&local)

will only materialize the local and extend its lifetime for the duration
of the `foo()` function call that the parameter is passed to.  The pointer
should not be persisted beyond that function call.

In the case of `bind()`, the local variable `sa` is only materialized for the
duration of the `UnsafePointer<sockaddr>()` initializer, and not any longer.
As soon as that initializer returns, the pointer is pointing to garbage.  That
pointer is passed to `bind()`, and the results are undefined.

This may work in practice because this local variable is a simple value type on
the stack, however, you should not come to rely on it.

To work around this, we can define a wrapper over `bind()` that accepts a
pointer of the correct type, keeping our pointer alive for the duration of its
use within the wrapper, namely, passing it on to the original `bind()`
function.  Here is that wrapper::

  func bind(socketFD: Int, socketAddressIn: UnsafePointer<sockaddr_in>) -> Int {
    let socketAddress = UnsafePointer<sockaddr>(socketAddressIn)
    return Int(bind(Int32(socketFD), socketAddress, socklen_t(sizeof(sockaddr_in))))
  }

  bind(serverSocket, &serverAddress)


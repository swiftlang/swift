# Remote Call Semantics

Understand how distributed method calls are dispatched, and how to opt into
synchronous, blocking remote calls.

## Overview

<!-- TODO: fill in the semantics of remote call dispatch here. -->

## Blocking Remote Calls

<!--
TODO(you): document '@remoteCall(blocking)' semantics.

Points to cover:
- what '@remoteCall(blocking)' means and when to use it
- that the distributed method may remain 'async'; the attribute changes how the
  remote IPC is performed, not the method's own effects
- how the actor system observes ``Distributed/RemoteCallTarget/isSynchronousBlockingCall``
  inside its 'remoteCall' / 'remoteCallVoid' to select a blocking IPC path
- the 'DistributedRemoteBlockingCalls' experimental feature requirement
- caveats around blocking the calling thread / thread starvation
-->

## Topics

### Blocking remote calls

- ``Distributed/RemoteCallTarget/isSynchronousBlockingCall``

// RUN: %target-swift-frontend -parse %s

// REQUIRES: objc_interop

import Dispatch

func getAnyValue<T>(_ opt: T?) -> T { return opt! }

// dispatch/io.h
let io = DispatchIO(type: .stream, fileDescriptor: 0, queue: DispatchQueue.main, cleanupHandler: { (error: Int32) -> () in fatalError() })
io.close(flags: .stop)
io.setInterval(interval: 0, flags: .strictInterval)

// dispatch/queue.h
let q = DispatchQueue(label: "", attributes: .serial)
_ = DispatchQueue(label: "", attributes: .concurrent)
_ = q.label
if #available(OSX 10.10, iOS 8.0, *) {
	_ = DispatchQueue.global(attributes: .qosUserInteractive)
	_ = DispatchQueue.global(attributes: .qosBackground)
	_ = DispatchQueue.global(attributes: .qosDefault)	
}

// dispatch/source.h
_ = DispatchSource.userDataAdd()
_ = DispatchSource.userDataOr()
_ = DispatchSource.machSend(port: mach_port_t(0), eventMask: [])
_ = DispatchSource.machReceive(port: mach_port_t(0))
_ = DispatchSource.memoryPressure(eventMask: [])
_ = DispatchSource.process(identifier: 0, eventMask: [])
_ = DispatchSource.read(fileDescriptor: 0)
_ = DispatchSource.signal(signal: SIGINT)
_ = DispatchSource.timer()
_ = DispatchSource.fileSystemObject(fileDescriptor: 0, eventMask: [])
_ = DispatchSource.write(fileDescriptor: 0)

// dispatch/time.h
_ = DispatchTime.now()
_ = DispatchTime.distantFuture

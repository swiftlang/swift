// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop
// UNSUPPORTED: OS=watchos


import Network
import Foundation
import StdlibUnittest


defer { runAllTests() }

var NetworkAPI = TestSuite("NetworkAPI")

#if !os(watchOS)

if #available(macOS 10.14, iOS 12.0, tvOS 12.0, *) {
	NetworkAPI.test("constants") {
		expectNotNil(NWConnection.ContentContext.defaultMessage)
		expectNotNil(NWConnection.ContentContext.finalMessage)
		expectNotNil(NWConnection.ContentContext.defaultStream)
		expectNotNil(NWParameters.tcp)
		expectNotNil(NWParameters.tls)
		expectNotNil(NWParameters.udp)
		expectNotNil(NWParameters.dtls)
		expectNotNil(IPv4Address.any)
		expectNotNil(IPv4Address.broadcast)
		expectNotNil(IPv4Address.loopback)
		expectNotNil(IPv4Address.allHostsGroup)
		expectNotNil(IPv4Address.allRoutersGroup)
		expectNotNil(IPv4Address.allReportsGroup)
		expectNotNil(IPv4Address.mdnsGroup)
		expectNotNil(IPv6Address.any)
		expectNotNil(IPv6Address.loopback)
		expectNotNil(IPv6Address.nodeLocalNodes)
		expectNotNil(IPv6Address.linkLocalNodes)
		expectNotNil(IPv6Address.linkLocalRouters)
		expectNotNil(NWEndpoint.Port.any)
		expectNotNil(NWEndpoint.Port.ssh)
		expectNotNil(NWEndpoint.Port.smtp)
		expectNotNil(NWEndpoint.Port.http)
		expectNotNil(NWEndpoint.Port.pop)
		expectNotNil(NWEndpoint.Port.imap)
		expectNotNil(NWEndpoint.Port.https)
		expectNotNil(NWEndpoint.Port.imaps)
		expectNotNil(NWEndpoint.Port.socks)
	}

	NetworkAPI.test("NWEndpoint") {
		let hostEndpoint = NWEndpoint.hostPort(host: "www.apple.com", port: .http)
		expectNotNil(hostEndpoint)
		expectNil(hostEndpoint.interface)

		let bonjourEndpoint = NWEndpoint.service(name: "myprinter", type: "_ipp._tcp", domain: "local", interface: nil)
		expectNotNil(bonjourEndpoint)
		expectNil(bonjourEndpoint.interface)

		let unixEndpoint = NWEndpoint.unix(path: "/foo/bar")
		expectNotNil(unixEndpoint)
		expectNil(unixEndpoint.interface)
	}

	NetworkAPI.test("NWEndpoint.Host") {
		var host = NWEndpoint.Host("www.apple.com")
		expectNotNil(host)
		expectNil(host.interface)

		host = NWEndpoint.Host("127.0.0.1")
		expectNotNil(host)
		expectNil(host.interface)

		host = NWEndpoint.Host("::1")
		expectNotNil(host)
		expectNil(host.interface)

		host = NWEndpoint.Host("::1%lo0")
		expectNotNil(host)
		expectNotNil(host.interface)
		if let interface = host.interface {
			expectEqual(interface.name, "lo0")
			expectEqual(interface.type, .loopback)
		}

		var ipv4Address = IPv4Address("127.0.0.1")
		expectNotNil(ipv4Address)
		expectNotNil(ipv4Address!.rawValue)
		expectEqual(ipv4Address!.rawValue.count, 4)
		expectNil(ipv4Address!.interface)
		expectTrue(ipv4Address!.isLoopback)

		let otherIPv4Address = IPv4Address("127.0.0.1")
		expectEqual(ipv4Address, otherIPv4Address)

		ipv4Address = IPv4Address("169.254.1.0")
		expectNotNil(ipv4Address)
		expectTrue(ipv4Address!.isLinkLocal)

		ipv4Address = IPv4Address("224.0.0.1")
		expectNotNil(ipv4Address)
		expectTrue(ipv4Address!.isMulticast)

		var ipv6Address = IPv6Address("::0")
		expectNotNil(ipv6Address)
		expectTrue(ipv6Address!.isAny)
		expectNotNil(ipv6Address!.rawValue)
		expectEqual(ipv6Address!.rawValue.count, 16)
		expectNil(ipv6Address!.interface)

		ipv6Address = IPv6Address("::1")
		expectNotNil(ipv6Address)
		expectTrue(ipv6Address!.isLoopback)
		expectNil(ipv6Address!.interface)

		ipv6Address = IPv6Address("::1%lo0")
		expectNotNil(ipv6Address)
		expectTrue(ipv6Address!.isLoopback)
		expectNotNil(ipv6Address!.interface)
		if let interface = ipv6Address!.interface {
			expectEqual(interface.name, "lo0")
			expectEqual(interface.type, .loopback)
		}

		ipv6Address = IPv6Address("::1.2.3.4")
		expectNotNil(ipv6Address)
		expectTrue(ipv6Address!.isIPv4Compatabile)

		ipv6Address = IPv6Address("::ffff:1.2.3.4")
		expectNotNil(ipv6Address)
		expectTrue(ipv6Address!.isIPv4Mapped)

		ipv4Address = ipv6Address!.asIPv4
		expectNotNil(ipv4Address)

		ipv6Address = IPv6Address("2002::1")
		expectNotNil(ipv6Address)
		expectTrue(ipv6Address!.is6to4)

		ipv6Address = IPv6Address("fe80::1")
		expectNotNil(ipv6Address)
		expectTrue(ipv6Address!.isLinkLocal)

		ipv6Address = IPv6Address("ff02::1")
		expectNotNil(ipv6Address)
		expectTrue(ipv6Address!.isMulticast)

		expectEqual(ipv6Address!.multicastScope, .linkLocal)

		// Try a bad multicast scope
		ipv6Address = IPv6Address("ff03::1")
		expectNotNil(ipv6Address)
		expectTrue(ipv6Address!.isMulticast)
		expectTrue(ipv6Address!.multicastScope != .linkLocal)
	}

	NetworkAPI.test("NWEndpoint.Port") {
		let port: NWEndpoint.Port = 1234
		expectNotNil(port)
		expectEqual(port.rawValue, 1234)

		expectEqual(NWEndpoint.Port.https, 443)
		expectEqual(NWEndpoint.Port("https")!.rawValue, 443)
	}

	NetworkAPI.test("NWParameters") {
		var parameters = NWParameters.tcp
		expectNotNil(parameters)
		expectTrue(parameters.defaultProtocolStack.internetProtocol is NWProtocolIP.Options)
		expectTrue(parameters.defaultProtocolStack.transportProtocol is NWProtocolTCP.Options)
		expectTrue(parameters.defaultProtocolStack.applicationProtocols.count == 0)

		parameters = parameters.copy()
		expectTrue(parameters.defaultProtocolStack.internetProtocol is NWProtocolIP.Options)
		expectTrue(parameters.defaultProtocolStack.transportProtocol is NWProtocolTCP.Options)
		expectTrue(parameters.defaultProtocolStack.applicationProtocols.count == 0)

		parameters = NWParameters.udp
		expectNotNil(parameters)
		expectTrue(parameters.defaultProtocolStack.transportProtocol is NWProtocolUDP.Options)
		expectTrue(parameters.defaultProtocolStack.applicationProtocols.count == 0)

		parameters = NWParameters.tls
		expectNotNil(parameters)
		expectTrue(parameters.defaultProtocolStack.transportProtocol is NWProtocolTCP.Options)
		expectTrue(parameters.defaultProtocolStack.applicationProtocols.count == 1)
		expectTrue(parameters.defaultProtocolStack.applicationProtocols[0] is NWProtocolTLS.Options)

		parameters.defaultProtocolStack.transportProtocol = NWProtocolUDP.Options()
		expectTrue(parameters.defaultProtocolStack.transportProtocol is NWProtocolUDP.Options)

		parameters = NWParameters.tls
		expectNotNil(parameters)
		expectTrue(parameters.defaultProtocolStack.transportProtocol is NWProtocolTCP.Options)

		parameters = NWParameters.dtls
		expectNotNil(parameters)
		expectTrue(parameters.defaultProtocolStack.transportProtocol is NWProtocolUDP.Options)
		expectTrue(parameters.defaultProtocolStack.applicationProtocols.count == 1)
		expectTrue(parameters.defaultProtocolStack.applicationProtocols[0] is NWProtocolTLS.Options)

		parameters = NWParameters(tls:nil, tcp:NWProtocolTCP.Options())
		expectNotNil(parameters)
		expectTrue(parameters.defaultProtocolStack.transportProtocol is NWProtocolTCP.Options)
		expectTrue(parameters.defaultProtocolStack.applicationProtocols.count == 0)

		parameters = NWParameters(dtls:nil, udp:NWProtocolUDP.Options())
		expectNotNil(parameters)
		expectTrue(parameters.defaultProtocolStack.transportProtocol is NWProtocolUDP.Options)
		expectTrue(parameters.defaultProtocolStack.applicationProtocols.count == 0)

		parameters = NWParameters(tls:NWProtocolTLS.Options(), tcp:NWProtocolTCP.Options())
		expectNotNil(parameters)
		expectTrue(parameters.defaultProtocolStack.transportProtocol is NWProtocolTCP.Options)
		expectTrue(parameters.defaultProtocolStack.applicationProtocols.count == 1)
		expectTrue(parameters.defaultProtocolStack.applicationProtocols[0] is NWProtocolTLS.Options)

		parameters = NWParameters(dtls:NWProtocolTLS.Options(), udp:NWProtocolUDP.Options())
		expectNotNil(parameters)
		expectTrue(parameters.defaultProtocolStack.transportProtocol is NWProtocolUDP.Options)
		expectTrue(parameters.defaultProtocolStack.applicationProtocols.count == 1)
		expectTrue(parameters.defaultProtocolStack.applicationProtocols[0] is NWProtocolTLS.Options)

		parameters = NWParameters()
		expectNotNil(parameters)
		expectNotNil(parameters.defaultProtocolStack)
		expectTrue(parameters.defaultProtocolStack.applicationProtocols.count == 0)
		expectNil(parameters.defaultProtocolStack.transportProtocol)

		parameters.defaultProtocolStack.transportProtocol = NWProtocolTCP.Options()

		expectNil(parameters.requiredInterface)

		expectEqual(parameters.requiredInterfaceType, NWInterface.InterfaceType.other)
		parameters.requiredInterfaceType = .wifi
		expectEqual(parameters.requiredInterfaceType, NWInterface.InterfaceType.wifi)

		expectTrue(parameters.prohibitedInterfaces == nil ||
				   parameters.prohibitedInterfaces!.count == 0)

		expectTrue(parameters.prohibitedInterfaceTypes == nil ||
				   parameters.prohibitedInterfaceTypes!.count == 0)
		parameters.prohibitedInterfaceTypes = [ .cellular ]
		expectTrue(parameters.prohibitedInterfaceTypes!.count == 1)
		expectEqual(parameters.prohibitedInterfaceTypes![0], .cellular)

		expectEqual(parameters.prohibitExpensivePaths, false)
		parameters.prohibitExpensivePaths = true;
		expectEqual(parameters.prohibitExpensivePaths, true)

		expectEqual(parameters.preferNoProxies, false)
		parameters.preferNoProxies = true;
		expectEqual(parameters.preferNoProxies, true)

		expectNil(parameters.requiredLocalEndpoint)
		parameters.requiredLocalEndpoint = NWEndpoint.hostPort(host: "127.0.0.1", port: 1234)
		expectNotNil(parameters.requiredLocalEndpoint)

		expectEqual(parameters.allowLocalEndpointReuse, false)
		parameters.allowLocalEndpointReuse = true;
		expectEqual(parameters.allowLocalEndpointReuse, true)

		expectEqual(parameters.acceptLocalOnly, false)
		parameters.acceptLocalOnly = true;
		expectEqual(parameters.acceptLocalOnly, true)

		expectEqual(parameters.serviceClass, NWParameters.ServiceClass.bestEffort)
		parameters.serviceClass = .background;
		expectEqual(parameters.serviceClass, NWParameters.ServiceClass.background)

		expectEqual(parameters.multipathServiceType, NWParameters.MultipathServiceType.disabled)
		parameters.multipathServiceType = .handover;
		expectEqual(parameters.multipathServiceType, NWParameters.MultipathServiceType.handover)

		expectEqual(parameters.expiredDNSBehavior, NWParameters.ExpiredDNSBehavior.systemDefault)
		parameters.expiredDNSBehavior = .allow;
		expectEqual(parameters.expiredDNSBehavior, NWParameters.ExpiredDNSBehavior.allow)

		expectEqual(parameters.allowFastOpen, false)
		parameters.allowFastOpen = true;
		expectEqual(parameters.allowFastOpen, true)

        expectEqual(parameters.includePeerToPeer, false)
        parameters.includePeerToPeer = true;
        expectEqual(parameters.includePeerToPeer, true)
	}

	NetworkAPI.test("NWProtocolTCP") {
		expectNotNil(NWProtocolTCP.definition)
		expectEqual(NWProtocolTCP.definition.name, "tcp")

		let options = NWProtocolTCP.Options()
		expectNotNil(options)

		expectEqual(options.noDelay, false)
		options.noDelay = true;
		expectEqual(options.noDelay, true)

		expectEqual(options.noPush, false)
		options.noPush = true;
		expectEqual(options.noDelay, true)

		expectEqual(options.noOptions, false)
		options.noOptions = true;
		expectEqual(options.noOptions, true)

		expectEqual(options.enableKeepalive, false)
		options.enableKeepalive = true;
		expectEqual(options.enableKeepalive, true)

		expectEqual(options.keepaliveCount, 0)
		options.keepaliveCount = 5;
		expectEqual(options.keepaliveCount, 5)

		expectEqual(options.keepaliveIdle, 0)
		options.keepaliveIdle = 5;
		expectEqual(options.keepaliveIdle, 5)

		expectEqual(options.keepaliveInterval, 0)
		options.keepaliveInterval = 5;
		expectEqual(options.keepaliveInterval, 5)

		expectEqual(options.maximumSegmentSize, 0)
		options.maximumSegmentSize = 500;
		expectEqual(options.maximumSegmentSize, 500)

		expectEqual(options.connectionTimeout, 0)
		options.connectionTimeout = 60;
		expectEqual(options.connectionTimeout, 60)

		expectEqual(options.persistTimeout, 0)
		options.persistTimeout = 60;
		expectEqual(options.persistTimeout, 60)

		expectEqual(options.connectionDropTime, 0)
		options.connectionDropTime = 60;
		expectEqual(options.connectionDropTime, 60)

		expectEqual(options.retransmitFinDrop, false)
		options.retransmitFinDrop = true;
		expectEqual(options.retransmitFinDrop, true)

		expectEqual(options.disableAckStretching, false)
		options.disableAckStretching = true;
		expectEqual(options.disableAckStretching, true)

		expectEqual(options.enableFastOpen, false)
		options.enableFastOpen = true;
		expectEqual(options.enableFastOpen, true)

		expectEqual(options.disableECN, false)
		options.disableECN = true;
		expectEqual(options.disableECN, true)
	}

	NetworkAPI.test("NWProtocolUDP") {
		expectNotNil(NWProtocolUDP.definition)
		expectEqual(NWProtocolUDP.definition.name, "udp")

		let options = NWProtocolUDP.Options()
		expectNotNil(options)

		expectEqual(options.preferNoChecksum, false)
		options.preferNoChecksum = true;
		expectEqual(options.preferNoChecksum, true)
	}

	NetworkAPI.test("NWProtocolIP") {
		expectNotNil(NWProtocolIP.definition)
		expectEqual(NWProtocolIP.definition.name, "ip")

		let parameters = NWParameters()
		let options = parameters.defaultProtocolStack.internetProtocol
		expectNotNil(options)

		expectTrue(options is NWProtocolIP.Options)

		let ipOptions = options as! NWProtocolIP.Options

		expectEqual(ipOptions.version, .any)
		ipOptions.version = .v6;
		expectEqual(ipOptions.version, .v6)

		expectEqual(ipOptions.hopLimit, 0)
		ipOptions.hopLimit = 5;
		expectEqual(ipOptions.hopLimit, 5)

		expectEqual(ipOptions.useMinimumMTU, false)
		ipOptions.useMinimumMTU = true;
		expectEqual(ipOptions.useMinimumMTU, true)

		expectEqual(ipOptions.disableFragmentation, false)
		ipOptions.disableFragmentation = true;
		expectEqual(ipOptions.disableFragmentation, true)

		let metadata = NWProtocolIP.Metadata()
		expectNotNil(metadata)

		expectEqual(metadata.ecn, .nonECT)
		metadata.ecn = .ect0;
		expectEqual(metadata.ecn, .ect0)

		expectEqual(metadata.serviceClass, .bestEffort)
		metadata.serviceClass = .background;
		expectEqual(metadata.serviceClass, .background)

        expectEqual(metadata.receiveTime, 0)
	}

	NetworkAPI.test("NWProtocolTLS") {
		expectNotNil(NWProtocolTLS.definition)
		expectEqual(NWProtocolTLS.definition.name, "tls")

		let options = NWProtocolTLS.Options()
		expectNotNil(options)
		expectNotNil(options.securityProtocolOptions)
	}

	NetworkAPI.test("NWPath")
		.skip(.iOSSimulatorAny("Path not fully supported on simulator"))
		.skip(.tvOSSimulatorAny("Path not fully supported on simulator"))
		.code {
		let testQueue = DispatchQueue(label: "testQueue")
		let group = DispatchGroup()

		let monitor = NWPathMonitor()
		expectNotNil(monitor)

		monitor.pathUpdateHandler = { (newPath) in
			expectNotNil(newPath)
			group.leave()
		}

		group.enter();
		monitor.start(queue: testQueue)

		let path = monitor.currentPath
		expectNotNil(path)

		let result = group.wait(timeout: DispatchTime.now() + .seconds(10))
		expectTrue(result == .success)

		expectEqual(monitor.queue, testQueue)

		expectNil(path.localEndpoint)
		expectNil(path.remoteEndpoint)

		for interface in path.availableInterfaces {
			expectNotNil(interface.name)
			expectTrue(interface.index != 0)
		}

		monitor.cancel()

		let wifiMonitor = NWPathMonitor(requiredInterfaceType: .wifi)
		expectNotNil(wifiMonitor)

		wifiMonitor.start(queue: testQueue)

		let wifiPath = wifiMonitor.currentPath
		expectNotNil(wifiPath)

		if wifiPath.status == .satisfied {
			expectTrue(wifiPath.usesInterfaceType(.wifi))
			expectTrue(wifiPath.supportsIPv4 || wifiPath.supportsIPv6 || wifiPath.supportsDNS)
			expectTrue(wifiPath.availableInterfaces.count > 0)
			var someInterfaceWiFi = false
			for interface in wifiPath.availableInterfaces {
				if (interface.type == .wifi) {
					someInterfaceWiFi = true
					break
				}
			}
			expectTrue(someInterfaceWiFi)
		}

		wifiMonitor.cancel()

		let loopbackMonitor = NWPathMonitor(requiredInterfaceType: .loopback)
		expectNotNil(loopbackMonitor)

		loopbackMonitor.start(queue: testQueue)

		let loopbackPath = loopbackMonitor.currentPath
		expectNotNil(loopbackPath)

		expectTrue(!loopbackPath.isExpensive)

		loopbackMonitor.cancel()
	}

	NetworkAPI.test("NWListener failure")
		.skip(.iOSSimulatorAny("Path not fully supported on simulator"))
		.skip(.tvOSSimulatorAny("Path not fully supported on simulator"))
		.code {
		let parameters: NWParameters = .tcp
		parameters.requiredLocalEndpoint = NWEndpoint.hostPort(host: "127.0.0.1", port: 1234)

		var listener: NWListener? = nil
		do {
			listener = try NWListener(using: parameters, on: 2345)
		} catch {
			print("Received listener error: \(error).")
		}
		expectNil(listener)
	}

	NetworkAPI.test("NWListener")
		.skip(.iOSSimulatorAny("Path not fully supported on simulator"))
		.skip(.tvOSSimulatorAny("Path not fully supported on simulator"))
		.code {
		let testQueue = DispatchQueue(label: "testQueue")
		let group = DispatchGroup()
		let advertiseGroup = DispatchGroup()

		let listener = try! NWListener(using: .tcp, on: 1234)
		expectNotNil(listener)

		listener.service = NWListener.Service(type: "_ipp._tcp")

		listener.stateUpdateHandler = { (newState) in
			switch(newState) {
			case .ready:
				group.leave()
			case .failed(let error):
				expectNotNil(error)
				listener.cancel()
			case .cancelled:
				group.leave()
			default:
				break
			}
		}

		listener.newConnectionHandler = { (newConn) in
			expectNotNil(newConn)
			newConn.forceCancel()
		}

		listener.serviceRegistrationUpdateHandler = { (serviceChange) in
			switch(serviceChange) {
			case .add(let endpoint):
				expectNotNil(endpoint)
			case .remove(let endpoint):
				expectNotNil(endpoint)
			}
			advertiseGroup.leave()
		}

		group.enter()
		advertiseGroup.enter()
		listener.start(queue: testQueue)

		// Wait for ready
		var result = group.wait(timeout: DispatchTime.now() + .seconds(10))
		expectTrue(result == .success)

		// Wait for advertise
		result = advertiseGroup.wait(timeout: DispatchTime.now() + .seconds(10))
		expectTrue(result == .success)

		expectEqual(listener.queue, testQueue)
		expectTrue(listener.parameters.defaultProtocolStack.transportProtocol is NWProtocolTCP.Options)
		expectEqual(listener.port, 1234)

		group.enter()
		listener.cancel()

		// Wait for cancelled
		result = group.wait(timeout: DispatchTime.now() + .seconds(10))
		expectTrue(result == .success)
	}

	NetworkAPI.test("NWConnection creation") {
		var connection = NWConnection(host: "localhost", port: 2345, using:.tcp)
		expectNotNil(connection)

		connection = NWConnection(to: .hostPort(host: "localhost", port: 2345), using:.tcp)
		expectNotNil(connection)

		connection = NWConnection(to: .service(name: "myprinter", type: "_ipp._tcp", domain: "local", interface: nil), using:.tcp)
		expectNotNil(connection)

		connection = NWConnection(to: .unix(path: "/foo/bar"), using:.tcp)
		expectNotNil(connection)
	}

	NetworkAPI.test("NWConnection Waiting Reset")
		.skip(.iOSSimulatorAny("Path not fully supported on simulator"))
		.skip(.tvOSSimulatorAny("Path not fully supported on simulator"))
		.code {
		let testQueue = DispatchQueue(label: "testQueue")
		let group = DispatchGroup()

		let connection = NWConnection(host: "localhost", port: 3456, using:.tls)
		expectNotNil(connection)

		connection.stateUpdateHandler = { (newState) in
			switch(newState) {
			case .waiting(let error):
				expectNotNil(error)
				switch(error) {
				case .posix(let code):
					expectEqual(code, .ECONNREFUSED)
				default:
					break
				}
				group.leave()
			case .failed(let error):
				expectNotNil(error)
				connection.cancel()
			case .cancelled:
				group.leave()
			default:
				break
			}
		}

		group.enter()
		connection.start(queue: testQueue)

		// Wait for waiting
		var result = group.wait(timeout: DispatchTime.now() + .seconds(10))
		expectTrue(result == .success)

		connection.restart()

		group.enter()
		connection.cancel()

		// Wait for cancelled
		result = group.wait(timeout: DispatchTime.now() + .seconds(10))
		expectTrue(result == .success)
	}

	NetworkAPI.test("NWConnection Waiting DNS")
		.skip(.iOSSimulatorAny("Path not fully supported on simulator"))
		.skip(.tvOSSimulatorAny("Path not fully supported on simulator"))
		.code {
		let testQueue = DispatchQueue(label: "testQueue")
		let group = DispatchGroup()

		let connection = NWConnection(host: "foobar.fake.apple.com", port: .https, using:.tls)
		expectNotNil(connection)

		connection.stateUpdateHandler = { (newState) in
			switch(newState) {
			case .waiting(let error):
				expectNotNil(error)
				switch(error) {
				case .dns(let code):
					expectEqual(code, DNSServiceErrorType(kDNSServiceErr_NoSuchRecord))
				default:
					break
				}
				group.leave()
			case .failed(let error):
				expectNotNil(error)
				connection.cancel()
			case .cancelled:
				group.leave()
			default:
				break
			}
		}

		group.enter()
		connection.start(queue: testQueue)

		// Wait for waiting
		var result = group.wait(timeout: DispatchTime.now() + .seconds(10))
		expectTrue(result == .success)

		group.enter()
		connection.cancel()

		// Wait for cancelled
		result = group.wait(timeout: DispatchTime.now() + .seconds(10))
		expectTrue(result == .success)
	}

	NetworkAPI.test("NWConnection TCP")
		.skip(.iOSSimulatorAny("Path not fully supported on simulator"))
		.skip(.tvOSSimulatorAny("Path not fully supported on simulator"))
		.code {
		let testQueue = DispatchQueue(label: "testQueue")
		let group = DispatchGroup()
		let viableGroup = DispatchGroup()
		let pathGroup = DispatchGroup()
		let sendGroup = DispatchGroup()
		let receiveGroup = DispatchGroup()

		let listener = try! NWListener(using: .tcp, on: 2345)
		expectNotNil(listener)

		var inboundConnection: NWConnection? = nil
		listener.newConnectionHandler = { (newConn) in
			expectNotNil(newConn)
			inboundConnection = newConn
			newConn.receive(minimumIncompleteLength: 5, maximumLength: 5) { (receivedContent, context, isComplete, receivedError) in
				expectTrue(!isComplete)
				expectNil(receivedError)
				expectNotNil(receivedContent)
				expectNotNil(context)
				receiveGroup.leave()
			}
			newConn.start(queue: testQueue)
		}
		listener.start(queue: testQueue)

		// Make sure connecting by address works
		let ipv4Address = IPv4Address("127.0.0.1")
		let connection = NWConnection(to: NWEndpoint.hostPort(host: .ipv4(ipv4Address!), port: 2345), using:.tcp)
		expectNotNil(connection)

		connection.stateUpdateHandler = { (newState) in
			switch(newState) {
			case .ready:
				group.leave()
			case .failed(let error):
				expectNotNil(error)
				connection.cancel()
			case .cancelled:
				group.leave()
			default:
				break
			}
		}

		connection.pathUpdateHandler = { (newPath) in
			expectNotNil(newPath)
			pathGroup.leave()
		}

		connection.viabilityUpdateHandler = { (isViable) in
			expectTrue(isViable)
			viableGroup.leave()
		}

		connection.betterPathUpdateHandler = { (betterPathAvailable) in
			expectTrue(!betterPathAvailable)
		}

		group.enter()
		viableGroup.enter()
		pathGroup.enter()
		receiveGroup.enter()
		connection.start(queue: testQueue)

		// Wait for ready
		var result = group.wait(timeout: DispatchTime.now() + .seconds(10))
		expectTrue(result == .success)

		let path = connection.currentPath
		expectNotNil(path)
		if let path = path {
			expectTrue(path.usesInterfaceType(.loopback))
			expectNotNil(path.localEndpoint)
			expectNotNil(path.remoteEndpoint)
		}

		let metadata = connection.metadata(definition: NWProtocolTCP.definition)
		expectNotNil(metadata)
		expectTrue(metadata is NWProtocolTCP.Metadata)

		let tcpMetadata = metadata as! NWProtocolTCP.Metadata
		expectEqual(tcpMetadata.availableReceiveBuffer, 0)
		expectEqual(tcpMetadata.availableSendBuffer, 0)

		// Wait for viable
		result = viableGroup.wait(timeout: DispatchTime.now() + .seconds(10))
		expectTrue(result == .success)

		// Wait for path
		result = pathGroup.wait(timeout: DispatchTime.now() + .seconds(10))
		expectTrue(result == .success)

		expectEqual(connection.queue, testQueue)
		expectTrue(connection.parameters.defaultProtocolStack.transportProtocol is NWProtocolTCP.Options)
		expectNotNil(connection.endpoint)

		sendGroup.enter()

		let sendContent: Data = "hello".data(using: .utf8)!
		connection.send(content: sendContent, isComplete: false, completion: .contentProcessed({ (sendError) in
			expectNil(sendError)
			sendGroup.leave()
		}))

		// Wait for send
		result = sendGroup.wait(timeout: DispatchTime.now() + .seconds(10))
		expectTrue(result == .success)

		// Wait for receive
		result = receiveGroup.wait(timeout: DispatchTime.now() + .seconds(10))
		expectTrue(result == .success)

		// Send complete
		connection.send(content: sendContent, contentContext: .finalMessage, isComplete: true, completion: .idempotent)

		group.enter()
		connection.cancel()

		// Wait for cancelled
		result = group.wait(timeout: DispatchTime.now() + .seconds(10))
		expectTrue(result == .success)

		if let inboundConnection = inboundConnection {
			inboundConnection.forceCancel()
		}
		listener.cancel()
	}

	NetworkAPI.test("NWConnection UDP")
		.skip(.iOSSimulatorAny("Path not fully supported on simulator"))
		.skip(.tvOSSimulatorAny("Path not fully supported on simulator"))
		.code {
		let testQueue = DispatchQueue(label: "testQueue")
		let group = DispatchGroup()
		let cancelGroup = DispatchGroup()
		let receiveGroup = DispatchGroup()

		var inboundConnection: NWConnection? = nil
		let listener = try! NWListener(using: .udp, on: 4567)
		expectNotNil(listener)

		listener.newConnectionHandler = { (newConn) in
			expectNotNil(newConn)
			inboundConnection = newConn
			newConn.receiveMessage() { (receivedContent, context, isComplete, receivedError) in
				expectTrue(isComplete)
				expectNil(receivedError)
				expectNotNil(receivedContent)
				expectNotNil(context)
				receiveGroup.leave()
			}
			newConn.start(queue: testQueue)
		}
		listener.start(queue: testQueue)

		let connection = NWConnection(host: "localhost", port: 4567, using:.udp)
		expectNotNil(connection)

		connection.stateUpdateHandler = { (newState) in
			switch(newState) {
			case .ready:
				group.leave()
			case .failed(let error):
				expectNotNil(error)
				connection.cancel()
			case .cancelled:
				cancelGroup.leave()
			default:
				break
			}
		}

		group.enter()
		receiveGroup.enter()
		connection.start(queue: testQueue)

		// Wait for ready
		var result = group.wait(timeout: DispatchTime.now() + .seconds(10))
		expectTrue(result == .success)

		expectEqual(connection.queue, testQueue)
		expectTrue(connection.parameters.defaultProtocolStack.transportProtocol is NWProtocolUDP.Options)
		expectNotNil(connection.endpoint)

		let ipMetadata = NWProtocolIP.Metadata()
		ipMetadata.ecn = .ect0
		let sendContext = NWConnection.ContentContext(identifier: "sendHello", expiration: 5000, priority: 1.0, isFinal: false, antecedent: nil, metadata: [ipMetadata])

		expectNotNil(sendContext)
		expectNotNil(sendContext.protocolMetadata)
		expectNil(sendContext.antecedent)
		expectEqual(sendContext.expirationMilliseconds, 5000)
		expectEqual(sendContext.relativePriority, 1.0)
		expectEqual(sendContext.isFinal, false)
		expectEqual(sendContext.identifier, "sendHello")

		let sendContent: Data = "hello".data(using: .utf8)!

		connection.batch {
			connection.send(content: sendContent, contentContext: sendContext, completion: .idempotent)
		}

		// Wait for receive
		result = receiveGroup.wait(timeout: DispatchTime.now() + .seconds(10))
		expectTrue(result == .success)

		cancelGroup.enter()

		connection.cancelCurrentEndpoint()
		connection.cancel()

		// Wait for cancelled
		result = cancelGroup.wait(timeout: DispatchTime.now() + .seconds(10))
		expectTrue(result == .success)

		if let inboundConnection = inboundConnection {
			inboundConnection.forceCancel()
		}
		listener.cancel()
	}
}

#endif

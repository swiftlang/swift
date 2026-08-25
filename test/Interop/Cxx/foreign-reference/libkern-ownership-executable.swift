// RUN: %target-run-simple-swift(-I %S/Inputs -I %swift_src_root/lib/ClangImporter/SwiftBridging -cxx-interoperability-mode=default -enable-experimental-feature LibkernOwnershipConventions -Xfrontend -disable-availability-checking )

// REQUIRES: swift_feature_LibkernOwnershipConventions

// REQUIRES: executable_test

// This test asserts that LIBKERN_RETURNS_RETAINED and LIBKERN_RETURNS_NOT_RETAINED are honored.
// The retain/release counts checked below are only correct at -Onone.
//
// UNSUPPORTED: swift_test_mode_optimize
// UNSUPPORTED: swift_test_mode_optimize_size
// UNSUPPORTED: swift_test_mode_optimize_unchecked
// UNSUPPORTED: swift_test_mode_optimize_with_implicit_dynamic

import LibkernOwnership
import StdlibUnittest

var Tests = TestSuite("LibkernOwnershipAttributes")

let manager = ObjectManager.get()

Tests.test("Create a new service") {
  manager.reset()
  expectEqual(0, manager.getTotalRetains())
  expectEqual(0, manager.getTotalReleases())

  do {
    let service = Service.withID(7)
    expectEqual(7, service.getID())
  }

  expectEqual(manager.getTotalRetains() + 1, manager.getTotalReleases())
}

Tests.test("Attach service") {
  manager.reset()
  expectEqual(0, manager.getTotalRetains())
  expectEqual(0, manager.getTotalReleases())

  do {
    let service = Service.withID(11)
    expectEqual(11, service.getID())
    expectEqual(manager.getTotalRetains(), manager.getTotalReleases())

    do {
      let provider = Service.withID(13)
      expectEqual(manager.getTotalRetains(), manager.getTotalReleases())

      service.attach(provider)
      expectEqual(manager.getTotalRetains(), manager.getTotalReleases() + 2)

      do {
        let serviceProvider = service.getProvider()

        expectTrue(checkEqual(provider, serviceProvider))
        expectEqual(manager.getTotalRetains(), manager.getTotalReleases() + 3)
      }

      expectEqual(manager.getTotalRetains(), manager.getTotalReleases() + 2)

      do {
        let serviceProvider = service.__getProvider()

        expectTrue(checkEqual(provider, serviceProvider))
        expectEqual(manager.getTotalRetains(), manager.getTotalReleases() + 3)
      }

      expectEqual(manager.getTotalRetains(), manager.getTotalReleases() + 2)

      service.detach()
      expectEqual(manager.getTotalRetains(), manager.getTotalReleases())
    }

    expectEqual(manager.getTotalRetains() + 1, manager.getTotalReleases())
  }

  expectEqual(manager.getTotalRetains() + 2, manager.getTotalReleases())
}

Tests.test("Probe service") {
  manager.reset()

  do {
    let service = Service.withID(17)
    expectEqual(manager.getTotalRetains(), manager.getTotalReleases())

    let provider = Service.withID(19)
    expectEqual(manager.getTotalRetains(), manager.getTotalReleases())

    var score: Int32 = 41
    let probed = service.probe(provider, &score)

    expectEqual(manager.getTotalRetains(), manager.getTotalReleases() + 1)

    expectEqual(42, score)
    expectTrue(checkEqual(service, probed))
  }

  expectEqual(manager.getTotalRetains() + 2, manager.getTotalReleases())
}

Tests.test("Copy service") {
  manager.reset()

  do {
    let service = Service.withID(23)
    expectEqual(manager.getTotalRetains(), manager.getTotalReleases())

    let copy = service.copyService()
    expectFalse(checkEqual(service, copy))
    expectEqual(manager.getTotalRetains(), manager.getTotalReleases())
  }

  expectEqual(manager.getTotalRetains() + 2, manager.getTotalReleases())

  manager.reset()
  do {
    let service = Service.withID(29)
    let copy = Service.getCopyOfService(service)
    expectEqual(manager.getTotalRetains(), manager.getTotalReleases())
    expectFalse(checkEqual(service, copy))
  }

  expectEqual(2, manager.getTotalRetains())
  expectEqual(4, manager.getTotalReleases())

  manager.reset()
  do {
    let service = Service.withID(31)
    let copy = copyServiceFreeFunction(service)
    expectEqual(manager.getTotalRetains(), manager.getTotalReleases())
    expectFalse(checkEqual(service, copy))
  }

  expectEqual(manager.getTotalRetains() + 2, manager.getTotalReleases())
}

Tests.test("Unannotated methods returning an OSObject subclass return +1 unless the name starts with 'get'") {
  manager.reset()

  do {
    let service = Service.noAnnotationWithID(37)
    expectEqual(37, service.getID())
    expectEqual(manager.getTotalRetains(), manager.getTotalReleases())

    do {
      let same = service.virtualNoAnnotationCopyService()
      expectEqual(service.getID(), same.getID())
      expectEqual(manager.getTotalRetains(), manager.getTotalReleases())
    }

    expectEqual(manager.getTotalRetains() + 1, manager.getTotalReleases())
  }

  expectEqual(manager.getTotalRetains() + 2, manager.getTotalReleases())
}

Tests.test("Missing annotation on non-OSObject subclass") {
  manager.reset()

  do {
    let service = NonOSService.noAnnotationWithID(37)
    expectEqual(manager.getTotalRetains(), manager.getTotalReleases() + 1)
  }

  expectEqual(manager.getTotalRetains(), manager.getTotalReleases())
}

Tests.test("OSIterator is a special case") {
  manager.reset()

  do {
    let iterator = OSIterator.getIterator()
    expectEqual(manager.getTotalRetains(), manager.getTotalReleases())
  }

  expectEqual(manager.getTotalRetains() + 1, manager.getTotalReleases())

  manager.reset()

  do {
    let iterator = OSCollectionIterator.getCollectionIterator()
    expectEqual(manager.getTotalRetains(), manager.getTotalReleases())
  }

  expectEqual(manager.getTotalRetains() + 1, manager.getTotalReleases())
}

Tests.test("DerivedService") {
  manager.reset()

  do {
    let service = DerivedService.derivedWithID(41)
    expectEqual(manager.getTotalRetains(), manager.getTotalReleases())

    do {
      let provider = service.getProvider()
      expectEqual(manager.getTotalRetains(), manager.getTotalReleases())
    }

    expectEqual(manager.getTotalRetains(), manager.getTotalReleases())
  }

  expectEqual(manager.getTotalRetains() + 1, manager.getTotalReleases())
}

runAllTests()

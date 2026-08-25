// RUN: %target-swift-emit-silgen -I %S/Inputs -I %swift_src_root/lib/ClangImporter/SwiftBridging -cxx-interoperability-mode=default -enable-experimental-feature LibkernOwnershipConventions -disable-availability-checking %s | %FileCheck %s

// REQUIRES: swift_feature_LibkernOwnershipConventions

import LibkernOwnership

let service = Service.withID(7)
// CHECK: sil {{.*}}[clang Service.withID] {{.*}} -> @owned Service

var score: Int32 = 5
_ = service.probe(service, &score)
// CHECK: sil {{.*}}[clang Service.probe] {{.*}} -> Optional<Service>

_ = service.getProvider()
// CHECK: sil {{.*}}[clang Service.getProvider] {{.*}} -> Optional<Service>

_ = service.__getProvider()
// CHECK: sil {{.*}}[clang Service.__getProvider] {{.*}} -> Optional<Service>

_ = service.copyService()
// CHECK: sil {{.*}}__synthesizedVirtualCall_copyService{{.*}}[clang Service.copyService] {{.*}} -> @owned Service
_ = Service.getCopyOfService(service)
// CHECK: sil {{.*}}[clang Service.getCopyOfService] {{.*}} -> @owned Service

_ = copyServiceFreeFunction(service)
// CHECK: sil {{.*}}[clang copyServiceFreeFunction] {{.*}} -> @owned Service

_ = Service.noAnnotationWithID(11)
// CHECK: sil {{.*}}[clang Service.noAnnotationWithID] {{.*}} -> @owned Service

_ = service.virtualNoAnnotationCopyService()
// CHECK: sil {{.*}}[clang Service.virtualNoAnnotationCopyService] {{.*}} -> @owned Service

_ = NonOSService.noAnnotationWithID(11)
// CHECK: sil {{.*}}[clang NonOSService.noAnnotationWithID] {{.*}} -> NonOSService

_ = OSIterator.getIterator()
// CHECK: sil {{.*}}[clang OSIterator.getIterator] {{.*}} -> @owned OSIterator

_ = OSCollectionIterator.getCollectionIterator()
// CHECK: sil {{.*}}[clang OSCollectionIterator.getCollectionIterator] {{.*}} -> @owned OSCollectionIterator

let derived = DerivedService.derivedWithID(19)
_ = derived.getProvider()
// CHECK: sil {{.*}}[clang DerivedService.__synthesizedBaseCall___synthesizedVirtualCall_getProvider{{.*}} -> Optional<Service>

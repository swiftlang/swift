// RUN: %target-swift-emit-silgen -I %S/Inputs -I %swift_src_root/lib/ClangImporter/SwiftBridging -cxx-interoperability-mode=default -disable-availability-checking %s | %FileCheck %s

import LibkernOwnership

let service = Service.withID(7)
// CHECK: sil {{.*}}[clang Service.withID] {{.*}} -> @owned Service

var score: Int32 = 5
_ = service.probe(service, &score)
// CHECK: sil {{.*}}[clang Service.probe] {{.*}} -> Optional<Service>

_ = service.getProvider()
// CHECK: sil {{.*}}[clang Service.getProvider] {{.*}} -> Optional<Service>

_ = service.copyService()
// CHECK: sil {{.*}}__synthesizedVirtualCall_copyService{{.*}}[clang Service.copyService] {{.*}} -> @owned Service
_ = Service.getCopyOfService(service)
// CHECK: sil {{.*}}[clang Service.getCopyOfService] {{.*}} -> @owned Service

_ = copyServiceFreeFunction(service)
// CHECK: sil {{.*}}[clang copyServiceFreeFunction] {{.*}} -> @owned Service

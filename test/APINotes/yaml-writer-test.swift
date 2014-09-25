# RUN: %swift-ide-test -generate-api-annotation -o %t-AppKit.apinotes AppKit

# RUN: %clang_apinotes -binary-to-yaml -o %t-AppKit.yaml %t-AppKit.apinotes 
# RUN: FileCheck %s < %t-AppKit.yaml

// CHECK: Name: AppKit
// CHECK: Classes:         
// CHECK:   - Name:            NSCell
// CHECK:     Availability:    available
// CHECK:     AvailabilityMsg: ''
// CHECK:     Methods:         
// CHECK:       - Selector:        init
// CHECK:         MethodKind:      Instance
// CHECK:         Availability:    available
// CHECK:         AvailabilityMsg: ''
// CHECK:         DesignatedInit:  true
// CHECK:       - Selector:        'initImageCell:'
// CHECK:         MethodKind:      Instance
// CHECK:         Availability:    available
// CHECK:         AvailabilityMsg: ''
// CHECK:         DesignatedInit:  true
// CHECK:       - Selector:        'initTextCell:'
// CHECK:         MethodKind:      Instance
// CHECK:         Availability:    available
// CHECK:         AvailabilityMsg: ''
// CHECK:         DesignatedInit:  true
// CHECK:       - Selector:        'initWithCoder:'
// CHECK:         MethodKind:      Instance
// CHECK:         Availability:    available
// CHECK:         AvailabilityMsg: ''
// CHECK:         DesignatedInit:  true
// CHECK:   - Name:            NSView
// CHECK:     AuditedForNullability: true
// CHECK:     Availability:    available
// CHECK:     AvailabilityMsg: ''
// CHECK:     Methods:         
// CHECK:       - Selector:        'addSubview:'
// CHECK:         MethodKind:      Instance
// CHECK:         Nullability:     [ N ]
// CHECK:         NullabilityOfRet: N
// CHECK:         Availability:    available
// CHECK:         AvailabilityMsg: ''
// CHECK:       - Selector:        'addSubview:positioned:relativeTo:'
// CHECK:         MethodKind:      Instance
// CHECK:         Nullability:     [ N, N, O ]
// CHECK:         NullabilityOfRet: N
// CHECK:         Availability:    available
// CHECK:         AvailabilityMsg: ''
// CHECK:       - Selector:        'beginDraggingSessionWithItems:event:source:'
// CHECK:         MethodKind:      Instance
// CHECK:         Nullability:     [ U, U, N ]
// CHECK:         Availability:    available
// CHECK:         AvailabilityMsg: ''
// CHECK:     Properties:      
// CHECK:       - Name:            enclosingScrollView
// CHECK:         Nullability:     O
// CHECK:         Availability:    available
// CHECK:         AvailabilityMsg: ''
// CHECK:       - Name:            makeBackingLayer
// CHECK:         Nullability:     N
// CHECK:         Availability:    available
// CHECK:         AvailabilityMsg: ''

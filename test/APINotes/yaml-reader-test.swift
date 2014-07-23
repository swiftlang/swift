# RUN: %swift_driver_plain -apinotes -dump %s | FileCheck %s
---
Name:            UIKit
Availability:    iOS
AvailabilityMsg: iOSOnly
Classes:
  - Name:            UIFont
    Availability:    iOS
    AvailabilityMsg: iOSOnly
    Methods:
      - Selector:        'fontWithName:size:'
        MethodKind:      Instance
        Nullability:     [ N ]
        NullabilityOfRet: O
        Availability:    iOS
        AvailabilityMsg: iOSOnly
        DesignatedInit:  true
    Properties:
      - Name:            familyName
        Nullability:     N
        Availability:    iOS
        AvailabilityMsg: iOSOnly
      - Name:            fontName
        Nullability:     N
        Availability:    iOS
        AvailabilityMsg: iOSOnly
Protocols:
  - Name:            MyProto
    AuditedForNullability: true
  - Name:            MyProto2
    AuditedForNullability: true
Functions:
  - Name:        'globalFoo'
    Nullability:     [ N, N, O, S ]
    NullabilityOfRet: O
    Availability:    iOS
    AvailabilityMsg: iOSOnly
  - Name:        'globalFoo2'
    Nullability:     [ N, N, O, S ]
    NullabilityOfRet: O
Globals:
  - Name:            globalVar
    Nullability:     O
    Availability:    iOS
    AvailabilityMsg: iOSOnly
  - Name:            globalVar2
    Nullability:     O


# CHECK: Name:            UIKit
# CHECK: Availability:    iOS
# CHECK: AvailabilityMsg: iOSOnly
# CHECK: Classes:
# CHECK:   - Name:            UIFont
# CHECK:     Availability:    iOS
# CHECK:     AvailabilityMsg: iOSOnly
# CHECK:     Methods:
# CHECK:       - Selector:        'fontWithName:size:'
# CHECK:         MethodKind:      Instance
# CHECK:         Nullability:     [ N ]
# CHECK:         NullabilityOfRet: O
# CHECK:         Availability:    iOS
# CHECK:         AvailabilityMsg: iOSOnly
# CHECK:         DesignatedInit:  true
# CHECK:     Properties:
# CHECK:       - Name:            familyName
# CHECK:         Nullability:     N
# CHECK:         Availability:    iOS
# CHECK:         AvailabilityMsg: iOSOnly
# CHECK:       - Name:            fontName
# CHECK:         Nullability:     N
# CHECK:         Availability:    iOS
# CHECK:         AvailabilityMsg: iOSOnly
# CHECK:Protocols:       
# CHECK:  - Name:            MyProto
# CHECK:    AuditedForNullability: true
# CHECK:    Availability:    available
# CHECK:    AvailabilityMsg: ''
# CHECK:  - Name:            MyProto2
# CHECK:    AuditedForNullability: true
# CHECK:    Availability:    available
# CHECK:    AvailabilityMsg: ''
# CHECK:Functions:       
# CHECK:  - Name:            globalFoo
# CHECK:    Nullability:     [ N, N, O, U ]
# CHECK:    NullabilityOfRet: O
# CHECK:    Availability:    iOS
# CHECK:    AvailabilityMsg: iOSOnly
# CHECK:  - Name:            globalFoo2
# CHECK:    Nullability:     [ N, N, O, U ]
# CHECK:    NullabilityOfRet: O
# CHECK:    Availability:    available
# CHECK:    AvailabilityMsg: ''
# CHECK:Globals:         
# CHECK:  - Name:            globalVar
# CHECK:    Nullability:     O
# CHECK:    Availability:    iOS
# CHECK:    AvailabilityMsg: iOSOnly
# CHECK:  - Name:            globalVar2
# CHECK:    Nullability:     O
# CHECK:    Availability:    available
# CHECK:    AvailabilityMsg: 

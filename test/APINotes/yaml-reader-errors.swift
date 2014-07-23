# RUN: not %swift_driver_plain -apinotes -yaml-to-binary -o %t.apinotesc %s > %t.err 2>&1
# RUN: FileCheck %s < %t.err

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
# CHECK: Duplicate definition of method '-[UIFont fontWithName:size:]'
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
# CHECK: Duplicate definition of property 'UIFont.familyName'
      - Name:            familyName
        Nullability:     N
        Availability:    iOS
        AvailabilityMsg: iOSOnly
# CHECK: Multiple definitions of class 'UIFont'
  - Name:            UIFont
Protocols:
  - Name:            MyProto
    AuditedForNullability: true
# CHECK: Multiple definitions of protocol 'MyProto'
  - Name:            MyProto
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

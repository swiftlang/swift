
struct __CE_GeneralNameType : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var GNT_OtherName: __CE_GeneralNameType { get }
var GNT_RFC822Name: __CE_GeneralNameType { get }
var GNT_DNSName: __CE_GeneralNameType { get }
var GNT_X400Address: __CE_GeneralNameType { get }
var GNT_DirectoryName: __CE_GeneralNameType { get }
var GNT_EdiPartyName: __CE_GeneralNameType { get }
var GNT_URI: __CE_GeneralNameType { get }
var GNT_IPAddress: __CE_GeneralNameType { get }
var GNT_RegisteredID: __CE_GeneralNameType { get }
typealias CE_GeneralNameType = __CE_GeneralNameType
struct __CE_OtherName {
  var typeId: CSSM_OID
  var value: cssm_data
  init()
  init(typeId typeId: CSSM_OID, value value: cssm_data)
}
struct __CE_GeneralName {
  var nameType: CE_GeneralNameType
  var berEncoded: CSSM_BOOL
  var name: cssm_data
  init()
  init(nameType nameType: CE_GeneralNameType, berEncoded berEncoded: CSSM_BOOL, name name: cssm_data)
}
struct __CE_GeneralNames {
  var numNames: uint32
  var generalName: UnsafeMutablePointer<__CE_GeneralName>!
  init()
  init(numNames numNames: uint32, generalName generalName: UnsafeMutablePointer<__CE_GeneralName>!)
}
struct __CE_AuthorityKeyID {
  var keyIdentifierPresent: CSSM_BOOL
  var keyIdentifier: cssm_data
  var generalNamesPresent: CSSM_BOOL
  var generalNames: UnsafeMutablePointer<__CE_GeneralNames>!
  var serialNumberPresent: CSSM_BOOL
  var serialNumber: cssm_data
  init()
  init(keyIdentifierPresent keyIdentifierPresent: CSSM_BOOL, keyIdentifier keyIdentifier: cssm_data, generalNamesPresent generalNamesPresent: CSSM_BOOL, generalNames generalNames: UnsafeMutablePointer<__CE_GeneralNames>!, serialNumberPresent serialNumberPresent: CSSM_BOOL, serialNumber serialNumber: cssm_data)
}
var CE_KU_DigitalSignature: Int32 { get }
var CE_KU_NonRepudiation: Int32 { get }
var CE_KU_KeyEncipherment: Int32 { get }
var CE_KU_DataEncipherment: Int32 { get }
var CE_KU_KeyAgreement: Int32 { get }
var CE_KU_KeyCertSign: Int32 { get }
var CE_KU_CRLSign: Int32 { get }
var CE_KU_EncipherOnly: Int32 { get }
var CE_KU_DecipherOnly: Int32 { get }
var CE_CR_Unspecified: Int32 { get }
var CE_CR_KeyCompromise: Int32 { get }
var CE_CR_CACompromise: Int32 { get }
var CE_CR_AffiliationChanged: Int32 { get }
var CE_CR_Superseded: Int32 { get }
var CE_CR_CessationOfOperation: Int32 { get }
var CE_CR_CertificateHold: Int32 { get }
var CE_CR_RemoveFromCRL: Int32 { get }
struct __CE_ExtendedKeyUsage {
  var numPurposes: uint32
  var purposes: CSSM_OID_PTR!
  init()
  init(numPurposes numPurposes: uint32, purposes purposes: CSSM_OID_PTR!)
}
typealias CE_ExtendedKeyUsage = __CE_ExtendedKeyUsage
struct __CE_BasicConstraints {
  var cA: CSSM_BOOL
  var pathLenConstraintPresent: CSSM_BOOL
  var pathLenConstraint: uint32
  init()
  init(cA cA: CSSM_BOOL, pathLenConstraintPresent pathLenConstraintPresent: CSSM_BOOL, pathLenConstraint pathLenConstraint: uint32)
}
struct __CE_PolicyQualifierInfo {
  var policyQualifierId: CSSM_OID
  var qualifier: cssm_data
  init()
  init(policyQualifierId policyQualifierId: CSSM_OID, qualifier qualifier: cssm_data)
}
struct __CE_PolicyInformation {
  var certPolicyId: CSSM_OID
  var numPolicyQualifiers: uint32
  var policyQualifiers: UnsafeMutablePointer<__CE_PolicyQualifierInfo>!
  init()
  init(certPolicyId certPolicyId: CSSM_OID, numPolicyQualifiers numPolicyQualifiers: uint32, policyQualifiers policyQualifiers: UnsafeMutablePointer<__CE_PolicyQualifierInfo>!)
}
struct __CE_CertPolicies {
  var numPolicies: uint32
  var policies: UnsafeMutablePointer<__CE_PolicyInformation>!
  init()
  init(numPolicies numPolicies: uint32, policies policies: UnsafeMutablePointer<__CE_PolicyInformation>!)
}
var CE_CD_Unspecified: Int32 { get }
var CE_CD_KeyCompromise: Int32 { get }
var CE_CD_CACompromise: Int32 { get }
var CE_CD_AffiliationChanged: Int32 { get }
var CE_CD_Superseded: Int32 { get }
var CE_CD_CessationOfOperation: Int32 { get }
var CE_CD_CertificateHold: Int32 { get }
struct __CE_CrlDistributionPointNameType : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var CE_CDNT_FullName: __CE_CrlDistributionPointNameType { get }
var CE_CDNT_NameRelativeToCrlIssuer: __CE_CrlDistributionPointNameType { get }
struct __CE_DistributionPointName {
  struct __Unnamed_union_dpn {
    var fullName: UnsafeMutablePointer<__CE_GeneralNames>!
    var rdn: UnsafeMutablePointer<cssm_x509_rdn>!
    init(fullName fullName: UnsafeMutablePointer<__CE_GeneralNames>!)
    init(rdn rdn: UnsafeMutablePointer<cssm_x509_rdn>!)
    init()
  }
  var nameType: __CE_CrlDistributionPointNameType
  var dpn: __CE_DistributionPointName.__Unnamed_union_dpn
  init()
  init(nameType nameType: __CE_CrlDistributionPointNameType, dpn dpn: __CE_DistributionPointName.__Unnamed_union_dpn)
}
struct __CE_CRLDistributionPoint {
  var distPointName: UnsafeMutablePointer<__CE_DistributionPointName>!
  var reasonsPresent: CSSM_BOOL
  var reasons: uint8
  var crlIssuer: UnsafeMutablePointer<__CE_GeneralNames>!
  init()
  init(distPointName distPointName: UnsafeMutablePointer<__CE_DistributionPointName>!, reasonsPresent reasonsPresent: CSSM_BOOL, reasons reasons: uint8, crlIssuer crlIssuer: UnsafeMutablePointer<__CE_GeneralNames>!)
}
struct __CE_CRLDistPointsSyntax {
  var numDistPoints: uint32
  var distPoints: UnsafeMutablePointer<__CE_CRLDistributionPoint>!
  init()
  init(numDistPoints numDistPoints: uint32, distPoints distPoints: UnsafeMutablePointer<__CE_CRLDistributionPoint>!)
}
struct __CE_AccessDescription {
  var accessMethod: CSSM_OID
  var accessLocation: __CE_GeneralName
  init()
  init(accessMethod accessMethod: CSSM_OID, accessLocation accessLocation: __CE_GeneralName)
}
struct __CE_AuthorityInfoAccess {
  var numAccessDescriptions: uint32
  var accessDescriptions: UnsafeMutablePointer<__CE_AccessDescription>!
  init()
  init(numAccessDescriptions numAccessDescriptions: uint32, accessDescriptions accessDescriptions: UnsafeMutablePointer<__CE_AccessDescription>!)
}
struct __CE_SemanticsInformation {
  var semanticsIdentifier: UnsafeMutablePointer<CSSM_OID>!
  var nameRegistrationAuthorities: UnsafeMutablePointer<__CE_GeneralNames>!
  init()
  init(semanticsIdentifier semanticsIdentifier: UnsafeMutablePointer<CSSM_OID>!, nameRegistrationAuthorities nameRegistrationAuthorities: UnsafeMutablePointer<__CE_GeneralNames>!)
}
struct __CE_QC_Statement {
  var statementId: CSSM_OID
  var semanticsInfo: UnsafeMutablePointer<__CE_SemanticsInformation>!
  var otherInfo: UnsafeMutablePointer<cssm_data>!
  init()
  init(statementId statementId: CSSM_OID, semanticsInfo semanticsInfo: UnsafeMutablePointer<__CE_SemanticsInformation>!, otherInfo otherInfo: UnsafeMutablePointer<cssm_data>!)
}
struct __CE_QC_Statements {
  var numQCStatements: uint32
  var qcStatements: UnsafeMutablePointer<__CE_QC_Statement>!
  init()
  init(numQCStatements numQCStatements: uint32, qcStatements qcStatements: UnsafeMutablePointer<__CE_QC_Statement>!)
}
typealias CE_CrlNumber = uint32
typealias CE_DeltaCrl = uint32
struct __CE_IssuingDistributionPoint {
  var distPointName: UnsafeMutablePointer<__CE_DistributionPointName>!
  var onlyUserCertsPresent: CSSM_BOOL
  var onlyUserCerts: CSSM_BOOL
  var onlyCACertsPresent: CSSM_BOOL
  var onlyCACerts: CSSM_BOOL
  var onlySomeReasonsPresent: CSSM_BOOL
  var onlySomeReasons: uint8
  var indirectCrlPresent: CSSM_BOOL
  var indirectCrl: CSSM_BOOL
  init()
  init(distPointName distPointName: UnsafeMutablePointer<__CE_DistributionPointName>!, onlyUserCertsPresent onlyUserCertsPresent: CSSM_BOOL, onlyUserCerts onlyUserCerts: CSSM_BOOL, onlyCACertsPresent onlyCACertsPresent: CSSM_BOOL, onlyCACerts onlyCACerts: CSSM_BOOL, onlySomeReasonsPresent onlySomeReasonsPresent: CSSM_BOOL, onlySomeReasons onlySomeReasons: uint8, indirectCrlPresent indirectCrlPresent: CSSM_BOOL, indirectCrl indirectCrl: CSSM_BOOL)
}
struct __CE_GeneralSubtree {
  var base: UnsafeMutablePointer<__CE_GeneralNames>!
  var minimum: uint32
  var maximumPresent: CSSM_BOOL
  var maximum: uint32
  init()
  init(base base: UnsafeMutablePointer<__CE_GeneralNames>!, minimum minimum: uint32, maximumPresent maximumPresent: CSSM_BOOL, maximum maximum: uint32)
}
struct __CE_GeneralSubtrees {
  var numSubtrees: uint32
  var subtrees: UnsafeMutablePointer<__CE_GeneralSubtree>!
  init()
  init(numSubtrees numSubtrees: uint32, subtrees subtrees: UnsafeMutablePointer<__CE_GeneralSubtree>!)
}
struct __CE_NameConstraints {
  var permitted: UnsafeMutablePointer<__CE_GeneralSubtrees>!
  var excluded: UnsafeMutablePointer<__CE_GeneralSubtrees>!
  init()
  init(permitted permitted: UnsafeMutablePointer<__CE_GeneralSubtrees>!, excluded excluded: UnsafeMutablePointer<__CE_GeneralSubtrees>!)
}
struct __CE_PolicyMapping {
  var issuerDomainPolicy: CSSM_OID
  var subjectDomainPolicy: CSSM_OID
  init()
  init(issuerDomainPolicy issuerDomainPolicy: CSSM_OID, subjectDomainPolicy subjectDomainPolicy: CSSM_OID)
}
struct __CE_PolicyMappings {
  var numPolicyMappings: uint32
  var policyMappings: UnsafeMutablePointer<__CE_PolicyMapping>!
  init()
  init(numPolicyMappings numPolicyMappings: uint32, policyMappings policyMappings: UnsafeMutablePointer<__CE_PolicyMapping>!)
}
struct __CE_PolicyConstraints {
  var requireExplicitPolicyPresent: CSSM_BOOL
  var requireExplicitPolicy: uint32
  var inhibitPolicyMappingPresent: CSSM_BOOL
  var inhibitPolicyMapping: uint32
  init()
  init(requireExplicitPolicyPresent requireExplicitPolicyPresent: CSSM_BOOL, requireExplicitPolicy requireExplicitPolicy: uint32, inhibitPolicyMappingPresent inhibitPolicyMappingPresent: CSSM_BOOL, inhibitPolicyMapping inhibitPolicyMapping: uint32)
}
struct __CE_DataType : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var DT_AuthorityKeyID: __CE_DataType { get }
var DT_SubjectKeyID: __CE_DataType { get }
var DT_KeyUsage: __CE_DataType { get }
var DT_SubjectAltName: __CE_DataType { get }
var DT_IssuerAltName: __CE_DataType { get }
var DT_ExtendedKeyUsage: __CE_DataType { get }
var DT_BasicConstraints: __CE_DataType { get }
var DT_CertPolicies: __CE_DataType { get }
var DT_NetscapeCertType: __CE_DataType { get }
var DT_CrlNumber: __CE_DataType { get }
var DT_DeltaCrl: __CE_DataType { get }
var DT_CrlReason: __CE_DataType { get }
var DT_CrlDistributionPoints: __CE_DataType { get }
var DT_IssuingDistributionPoint: __CE_DataType { get }
var DT_AuthorityInfoAccess: __CE_DataType { get }
var DT_Other: __CE_DataType { get }
var DT_QC_Statements: __CE_DataType { get }
var DT_NameConstraints: __CE_DataType { get }
var DT_PolicyMappings: __CE_DataType { get }
var DT_PolicyConstraints: __CE_DataType { get }
var DT_InhibitAnyPolicy: __CE_DataType { get }
typealias CE_DataType = __CE_DataType
struct __CE_DataAndType {
  var type: CE_DataType
  var `extension`: CE_Data
  var critical: CSSM_BOOL
  init()
  init(type type: CE_DataType, extension extension: CE_Data, critical critical: CSSM_BOOL)
}

SECTIONS
{
  .swift2_protocol_conformances :
  {
    .swift2_protocol_conformances_start = . ;
    QUAD(SIZEOF(.swift2_protocol_conformances) - 8) ;
    *(.swift2_protocol_conformances) ;
  },
  .swift2_type_metadata :
  {
    .swift2_type_metadata_start = . ;
    QUAD(SIZEOF(.swift2_type_metadata) - 8) ;
    *(.swift2_type_metadata) ;
  }
}
INSERT AFTER .dtors

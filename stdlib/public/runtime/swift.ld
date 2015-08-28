SECTIONS
{
  .swift2_protocol_conformances :
  {
    .swift2_protocol_conformances_start = . ;
    QUAD(SIZEOF(.swift2_protocol_conformances) - 8) ;
    *(.swift2_protocol_conformances) ;
  }
}
INSERT AFTER .dtors

SECTIONS
{
  .swift1_protocol_conformances :
  {
    .swift1_protocol_conformances_start = . ;
    QUAD(SIZEOF(.swift1_protocol_conformances) - 8) ;
    *(.swift1_protocol_conformances) ;
  }
}
INSERT AFTER .dtors

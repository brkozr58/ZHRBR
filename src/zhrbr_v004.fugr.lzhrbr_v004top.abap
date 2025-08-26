* regenerated at 27.01.2022 17:16:32
FUNCTION-POOL ZHRBR_V004                 MESSAGE-ID SV.

* INCLUDE LZHRBR_V004D...                    " Local class definition
  INCLUDE LSVIMDAT                                . "general data decl.
  INCLUDE LZHRBR_V004T00                          . "view rel. data dcl.

DATA : BEGIN OF gs_text ,
     orngr_t  TYPE  text40 ,
     bukrs_t  TYPE  text40 ,
     lgart_t  TYPE  text40 ,
     lgart_net_t  TYPE  text40 ,
     END OF gs_text .

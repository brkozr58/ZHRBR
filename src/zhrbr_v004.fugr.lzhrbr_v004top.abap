* regenerated at 13.01.2020 21:03:20
FUNCTION-POOL zhrbr_v004                 MESSAGE-ID sv.

* INCLUDE LZHRBR_V004D...                    " Local class definition
INCLUDE lsvimdat                                . "general data decl.
INCLUDE lzhrbr_v004t00                          . "view rel. data dcl.

DATA : BEGIN OF gs_text ,
     orngr_t  TYPE  text40 ,
     bukrs_t  TYPE  text40 ,
     lgart_t  TYPE  text40 ,
     END OF gs_text .

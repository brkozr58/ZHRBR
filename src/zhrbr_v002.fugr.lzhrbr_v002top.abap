* regenerated at 13.01.2020 21:02:20
FUNCTION-POOL zhrbr_v002                 MESSAGE-ID sv.

* INCLUDE LZHRBR_V002D...                    " Local class definition
INCLUDE lsvimdat                                . "general data decl.
INCLUDE lzhrbr_v002t00                          . "view rel. data dcl.

DATA : BEGIN OF gs_text ,
     bukrs_t  TYPE text40 ,
     subty_t  TYPE text40 ,
     END OF gs_text .

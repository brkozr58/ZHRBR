* regenerated at 13.01.2020 21:04:45
FUNCTION-POOL zhrbr_v007                 MESSAGE-ID sv.

* INCLUDE LZHRBR_V007D...                    " Local class definition
INCLUDE lsvimdat                                . "general data decl.
INCLUDE lzhrbr_v007t00                          . "view rel. data dcl.

DATA : BEGIN OF gs_text ,
   bukrs_t  TYPE  text40 ,
   lgart_t  TYPE  text40 ,
   END OF gs_text .

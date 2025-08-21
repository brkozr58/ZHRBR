* regenerated at 13.01.2020 21:01:47
FUNCTION-POOL zhrbr_v001                 MESSAGE-ID sv.

* INCLUDE LZHRBR_V001D...                    " Local class definition
INCLUDE lsvimdat                                . "general data decl.
INCLUDE lzhrbr_v001t00                          . "view rel. data dcl.

DATA : BEGIN OF gs_text ,
     subty_t  TYPE  text40 ,
     kaluc_t  TYPE  text40 ,
     ksluc_t  TYPE  text40 ,
     END OF gs_text .
